module DMN.ODEs.System(
    equationSystem
) where

import qualified Data.Vector as V
import FRP.Yampa (Event(..), isEvent)
import DMN.Utils.Constants
import DMN.Utils.Structure
import DMN.ODEs.Equations


{-
    for fireVect:
        - ith P in nth assembly (Nsen)  <-> n * 20 + i
        - ith Ia in nth assembly (Nsen) <-> 160 + n * 20 + i
        - ith Ib in nth assembly (Nsen) <-> 320 + n * 20 + i
        - ith P_DMN                     <-> 480 + i
        - ith Ib_DMN                    <-> 500 + i
-}
equationSystem :: V.Vector (Event Int) -- whether Y fires at time t
               -> Bool                 -- whether there is sensory input
               -> V.Vector (V.Vector Double -> Double -> Double)
equationSystem fireVect p = equationSystem_aux V.// updateList
    where
        -- updateList for bulk update functions
        -- fill the ODEs for u_i^P(n;t), ODEs for r_j^X(n;t) (where X = P,Ia,Ib)
        -- and ODEs for r_i^X(t) (where X = P_DMN, Ib_DMN)
        -- based on equationSystem_aux
        updateList :: [(Int, V.Vector Double -> Double -> Double)]
        updateList = updataList_uP ++ (
                        updataList_rP ++ (
                            updataList_rIa ++ (
                                updataList_rIb ++ (
                                    updataList_rPDMN ++ 
                                        updataList_rIbDMN
                                )
                            )
                        )
                    )
            where
                -- Judging whether the cell is firing from the Event
                -- Caution: Event only implies the cell is in the refractory period, but only when it is in the absolute
                --          refractory period (number in the Event >= 10) can it be called a firing cell
                isFiring :: Event Int -> Bool
                isFiring NoEvent = False
                isFiring (Event n) = n >= 10

                -- for the following, `pos` is just the order for the current func (from 0 - 159/39/19)

                -- ODEs for u_i^P(n;t) while still lacking the parameter indicating whether there is sensory input
                uP_equations_base :: [Bool -> V.Vector Double -> Double -> Double]
                uP_equations_base = uP_i_n <$> [0..7] <*> [0..19]
                -- updataList for ODEs for u_i^P(n;t)
                updataList_uP :: [(Int, V.Vector Double -> Double -> Double)]  -- [(0, 0th u^P ODE),(1, 1th u^P ODE)...(159, 159th u^P ODE)]
                updataList_uP = snd $ foldr (\func (pos, res)-> (pos - 1, (pos, func p) : res)) (159,[]) uP_equations_base

                -- ODEs for r_j^P(n;t) while still lacking the parameter for whether the corresponding cell fires
                rP_equations_base :: [Bool -> V.Vector Double -> Double -> Double]
                rP_equations_base = rP_j_n <$> [0..7] <*> [0..19]
                -- updataList for ODEs for r_j^P(n;t) 
                updataList_rP :: [(Int, V.Vector Double -> Double -> Double)]  -- [(680, 0th r^P ODE),(681, 1th r^P ODE)...(839, 159th r^P ODE)]
                updataList_rP = let f func (pos, res) = let funcWithFireInfo = func (isFiring $ fireVect V.! (uP_start_pos + pos))
                                                        in (pos - 1, (rP_start_pos + pos, funcWithFireInfo) : res)
                                in snd $ foldr f (159,[]) rP_equations_base
                
                -- ODEs for r_j^Ia(n;t) while still lacking the parameter for whether the corresponding cell fires
                rIa_equations_base :: [Bool -> V.Vector Double -> Double -> Double]
                rIa_equations_base = rIa_j_n <$> [0..7] <*> [0..19]
                -- updataList for ODEs for r_j^Ia(n;t) 
                updataList_rIa :: [(Int, V.Vector Double -> Double -> Double)]  -- [(840, 0th r^Ia ODE),(841, 1th r^Ia ODE)...(999, 159th r^Ia ODE)]
                updataList_rIa = let f func (pos, res) = let funcWithFireInfo = func (isFiring $ fireVect V.! (uIa_start_pos + pos))
                                                         in (pos - 1, (rIa_start_pos + pos, funcWithFireInfo) : res)
                                 in snd $ foldr f (159,[]) rIa_equations_base

                -- ODEs for r_j^Ib(n;t) while still lacking the parameter for whether the corresponding cell fires
                rIb_equations_base :: [Bool -> V.Vector Double -> Double -> Double]
                rIb_equations_base = rIb_j_n <$> [0..7] <*> [0..19]
                -- updataList for ODEs for r_j^Ib(n;t) 
                updataList_rIb :: [(Int, V.Vector Double -> Double -> Double)]  -- [(1000, 0th r^Ia ODE),(1001, 1th r^Ia ODE)...(1159, 159th r^Ib ODE)]
                updataList_rIb = let f func (pos, res) = let funcWithFireInfo = func (isFiring $ fireVect V.! (uIb_start_pos + pos))
                                                         in (pos - 1, (rIb_start_pos + pos, funcWithFireInfo) : res)
                                 in snd $ foldr f (159,[]) rIb_equations_base
                
                -- ODEs for r_i^{P_DMN}(t) while still lacking the parameter for whether the corresponding cell fires
                rPDMN_equations_base :: [Bool -> V.Vector Double -> Double -> Double]
                rPDMN_equations_base = rPDMN_i <$> [0..19]
                -- updataList for ODEs for r_i^{P_DMN}(t)
                updataList_rPDMN :: [(Int, V.Vector Double -> Double -> Double)]  -- [(1340, 0th r^PDMN ODE),(1341, 1th r^PDMN ODE)...(1379, 39th r^PDMN ODE)]
                updataList_rPDMN = let f func (pos, res) = let funcWithFireInfo = func (isFiring $ fireVect V.! (uP_DMN_start_pos + pos))
                                                           in (pos - 1, (rP_DMN_start_pos + pos, funcWithFireInfo) : res)
                                   in snd $ foldr f (19,[]) rPDMN_equations_base

                -- ODEs for r_i^{Ib_DMN}(t) while still lacking the parameter for whether the corresponding cell fires
                rIbDMN_equations_base :: [Bool -> V.Vector Double -> Double -> Double]
                rIbDMN_equations_base = rIbDMN_i <$> [0..19]
                -- updataList for ODEs for r_i^{Ib_DMN}(t)
                updataList_rIbDMN :: [(Int, V.Vector Double -> Double -> Double)]  -- [(1380, 0th r^IbDMN ODE),(1381, 1th r^IbDMN ODE)...(1399, 19th r^IbDMN ODE)]
                updataList_rIbDMN = let f func (pos, res) = let funcWithFireInfo = func (isFiring $ fireVect V.! (uIb_DMN_start_pos + pos))
                                                            in (pos - 1, (rIb_DMN_start_pos + pos, funcWithFireInfo) : res)
                                    in snd $ foldr f (19,[]) rIbDMN_equations_base



-- auxiliary funtions, constructing ODE system vector with all equations which
-- not need a external parameters (i.e. whether sensory input exists OR whether certain cell fires at time t)
-- other ODE equations are filled with dumb temporarily 
equationSystem_aux :: V.Vector (V.Vector Double -> Double -> Double)
equationSystem_aux = V.fromList $ total_equation_list 
    where
        -- dumb functions for place-taking
        dumb :: V.Vector Double -> Double -> Double
        dumb = undefined

        dumb_n :: Int -> [V.Vector Double -> Double -> Double]
        dumb_n n = replicate n dumb

        -- 160 ODEs for u^{Ia}_i(n;t)
        uIa_equations :: [V.Vector Double -> Double -> Double]
        uIa_equations = uIa_i_n <$> [0..7] <*> [0..19]

        -- 160 ODEs for u^{Ib}_i(n;t)
        uIb_equations :: [V.Vector Double -> Double -> Double]
        uIb_equations = uIb_i_n <$> [0..7] <*> [0..19]

        -- 160 ODEs for u^{Gl}_i(n;t)
        uGl_equations :: [V.Vector Double -> Double -> Double]
        uGl_equations = uGl_i_n <$> [0..7] <*> [0..19]

        -- 40 ODEs for u_i^{P_DMN}(t)
        uPDMN_equations :: [V.Vector Double -> Double -> Double]
        uPDMN_equations = uPDMN_i <$> [0..19]

        -- 20 ODEs for u_i^{Ib_DMN}(t)
        uIbDMN_equations :: [V.Vector Double -> Double -> Double]
        uIbDMN_equations = uIbDMN_i <$> [0..19]

        -- 160 ODEs for r_{i,ext}^P(n;t)
        rext_equations :: [V.Vector Double -> Double -> Double]
        rext_equations = rext_i_n <$> [0..7] <*> [0..19]

        -- 160 ODEs for [GABA]_{i,ext}^P(n;t)
        gabaP_ext_equations :: [V.Vector Double -> Double -> Double]
        gabaP_ext_equations = gabaP_ext_i_n <$> [0..7] <*> [0..19]

        total_equation_list :: [V.Vector Double -> Double -> Double]
        total_equation_list = dumb_n 160 ++ (                                                   -- ODEs for u_i^P(n;t)
                                        uIa_equations ++ (
                                            uIb_equations ++ (
                                                uPDMN_equations ++ (
                                                     uIbDMN_equations ++ (
                                                        uGl_equations ++ (
                                                            dumb_n 520 ++ (                  -- ODEs for r_i^X(t) where X = P, Ia, Ib, P_DMN, Ib_DMN
                                                                    rext_equations ++
                                                                        gabaP_ext_equations
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
 


