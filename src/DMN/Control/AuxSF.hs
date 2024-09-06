module DMN.Control.AuxSF(
    oneStepCalc,
    updateResultFire
) where

import qualified Data.Vector as V
import DMN.ODEs.System
import DMN.Utils.RK
import DMN.Utils.Constants
import FRP.Yampa

-- unit: s
-- 100 microseconds = 10^2 * 10^{-6} s = 10^{-4} s = 0.0001
stepLength :: Double
stepLength = 0.0001

-- one step caculation, use the result at time t, output the primary result of time (t+1)(i.e. one more step)
-- the word "primary" means it has NOT taken firing into account
oneStepCalc_aux :: V.Vector Double        -- ODE system results at time t
                -> V.Vector (Event Int)   -- Fire vector at time t
                -> Bool                   -- whether sensory input exists
                -> Double                 -- time t, although in fact of no use
                -> V.Vector Double        -- ODE system results at time (t+1) 
oneStepCalc_aux resVect fireVect p t = fst $ rk_1S equationSystem_entire (resVect, t) stepLength
    where
        -- fully constructed ODE system
        equationSystem_entire = equationSystem fireVect p

-- Arrayize oneStepCalc_aux
oneStepCalc :: SF (V.Vector Double, V.Vector (Event Int), Bool, Double) (V.Vector Double)
oneStepCalc = arr $ uncurry4 oneStepCalc_aux
    where
        uncurry4 :: (a -> b -> c -> d -> res) -> (a, b, c, d) -> res 
        uncurry4 f = \(a, b, c, d) -> f a b c d


-- according to the previous firing event vector (at time t) and the current primary result (at time t+1)
-- update the primary result vector at time (t+1) into the final result at time (t+1) (with firing control)
-- and also output the firing event vector at time (t+1)
updateResultFire_aux :: (RandomGen g) => g              -- random number generator
                                      -> V.Vector Double         -- primary result at time (t+1)
                                      -> V.Vector (Event Int)    -- firing event vector at time t
                                      -> (V.Vector Double, V.Vector (Event Int))-- final result at time (t+1) and firing event vector at time (t+1)
updateResultFire_aux g resVect fireVect_pre = combine_bulk
    where 
        -- randomList for fire probability, if randomNum < prob, the cell fires
        randomList = take 520 $ randomRs (0.0,1.0) g

        -- one fold to obtain the update lists for fire event vector now and final result vector
        updateList_resVF_fireVN :: ([(Int, Double)],  -- updateList for resVectFinal
                                    [(Int, Event Int)])     -- updateList for fireVectNow
        updateList_resVF_fireVN = let f (position, reslst, firelst) randomNum
                                        -- P cell in Nsen
                                        | position < 160   = firing_state_update u_rest_P eta_P zeta_P
                                        -- Ia cell in Nsen
                                        | position < 320   = firing_state_update u_rest_Ia eta_Ia zeta_Ia
                                        -- Ib cell in Nsen
                                        | position < 480   = firing_state_update u_rest_Ib eta_Ib zeta_Ib
                                        -- P cell in DMN
                                        | position < 500   = firing_state_update u_rest_P eta_P_DMN zeta_P_DMN
                                        -- Ib cell in DMN
                                        | otherwise        = firing_state_update u_rest_Ib eta_Ib_DMN zeta_Ib_DMN 
                                        where
                                            firing_state_update :: Double -- resting potential of this cell type
                                                                -> Double -- eta
                                                                -> Double -- zeta
                                                                -> (Int, [(Int, Double)], [(Int, Event Int)])
                                            firing_state_update u_rest eta zeta = let fireState = fireVect_pre V.! position
                                                                                  in if (isEvent $ fireState)  -- whether the cell is in the refractory period (absolute (i.e. firing now) or relative)
                                                                                      then let timeRemain_pre = fromEvent fireState
                                                                                          in (if (timeRemain_pre >= 11)   -- whether the cell is still in the firing state, absolute refractory period
                                                                                              then (position + 1, (position, u_fire) : reslst, (position, tag fireState $ timeRemain_pre - 1) : firelst)
                                                                                              else (if (timeRemain_pre == 1) -- whether it (right now) is the last time point of relative refractory period
                                                                                                      then (position + 1, (position, u_rest) : reslst, (position, NoEvent) : firelst) -- if it is, set the firing state to NoEvent
                                                                                                      else (position + 1, (position, u_rest) : reslst, (position, tag fireState $ timeRemain_pre - 1) : firelst) -- if it isn't, set the firing state as the relative refractory period (Event 0-9)
                                                                                                  )
                                                                                              )
                                                                                      else let p = randomNum * (1 + exp(negate $ eta * ((resVect V.! position) - zeta)))
                                                                                          in if (p < 1)  -- p < 1 means randomNum * 1/prob < 1, i.e. randomNum < prob
                                                                                                          -- which means the cell fires
                                                                                              then (position + 1, (position, u_fire) : reslst, (position, Event 19) : firelst)
                                                                                              -- if also not fired at time t+1, donnot change reslst, set firelst to NoEvent
                                                                                              else (position + 1, reslst, (position, NoEvent) : firelst)
                                  in get23 $ foldl f (0,[],[]) randomList
                                      where get23 (a, b, c) = (b, c)

        -- use updateList for result vector and fire vector to get the final result vector at time (t+1) and firing vector at time (t+1)
        combine_bulk :: (V.Vector Double, V.Vector (Event Int))
        combine_bulk = case updateList_resVF_fireVN of 
                                (reslst, firelst) -> (resVect V.// reslst, fireVect_pre V.// firelst) 
        
-- Arrayize updateResultFire_aux
updateResultFire :: (RandomGen g) =>  SF (g, V.Vector Double, V.Vector (Event Int)) (V.Vector Double, V.Vector (Event Int))
updateResultFire = arr $ uncurry3 updateResultFire_aux
    where 
        uncurry3 :: (a -> b -> c -> res) -> (a, b, c) -> res
        uncurry3 f = \(a, b, c) -> f a b c
