module DMN.Utils.RK(
  rk_1S
) where


import Data.Vector as V

-- Runge-Kutta 4-4, arbitary differential equation systems, one step
rk_1S :: Vector (Vector Double -> Double -> Double) -- differential equations vectors, containing k-1 functions
                -- for functions here, the first parameter is the parameter lists for yn, zn, ... (all dependent parameters).
                -- So the first parameter's length should be k-1
                -- And the second parameter is the time t(the only independent parameter)
        -> (Vector Double, Double) -- results at time n, the first vector contains k-1 numbers(all dependent parameters). 
                                   -- the order of the vector should be consistent with the differential equation vectors
                                   -- i.e. for the ith differential equation's variable, its result at time n should be placed
                                   --      at the ith place in this vector
                                   -- The second parameter should be t (time)
        -> Double                  -- step length
        -> (Vector Double, Double) -- results at time n+1, the first vector contains k-1 numbers(all dependent parameters). 
                                   -- the second parameter is t+h (time)
rk_1S fs (xs,t) h = (,) (V.zipWith5 (\yn k1 k2 k3 k4 -> yn + h * (k1 + 2 * k2 + 2 * k3 + k4) / 6) xs ks1 ks2 ks3 ks4) $ t + h
  where 
        ks1 :: Vector Double  -- results of k1s, containing k-1 numbers
        ks1 = fmap (\f -> f xs t) fs

        ks2 :: Vector Double  -- results of k2s, containing k-1 numbers
        ks2 = fmap (\f -> f ks2_paraV (t + h / 2)) fs
          where ks2_paraV = V.zipWith (\k1 yn -> yn + h * k1 / 2) ks1 xs
                -- the dependent parameter list of ks2, containing k-1 numbers

        ks3 :: Vector Double  -- results of k3s, containing k-1 numbers
        ks3 = fmap (\f -> f ks3_paraV (t + h / 2)) fs 
          where ks3_paraV = V.zipWith (\k2 yn -> yn + h * k2 / 2) ks2 xs
                -- the dependent parameter list of ks3, containing k-1 numbers

        ks4 :: Vector Double
        ks4 = fmap (\f -> f ks4_paraV (t + h)) fs
          where ks4_paraV = V.zipWith (\k3 yn -> yn + h * k3) ks3 xs
                -- the dependent parameter list of ks4, containing k-1 numbers

-- Test
-- f1, f2 :: Vector Double -> Double -> Double
-- f1 = \ys t -> (negate 0.01) * (ys ! 0) - 99.99 * (ys ! 1)
-- f2 = \ys t -> (negate 100) * (ys ! 1)

-- rk_multiStep (V.fromList [f3,f4]) (V.fromList [6,4],0) 0.02 20
-- expects a results of [10.5396229463,11,7157806530]
-- f3,f4 :: Vector Double -> Double -> Double
-- f3 = \ys t -> (ys ! 0) + 2 * (ys ! 1)
-- f4 = \ys t -> 3 * (ys ! 0) + 2 * (ys ! 1)

-- rk_multiStep :: Vector (Vector Double -> Double -> Double)
--                 -> (Vector Double, Double) -- result at time n
--                 -> Double
--                 -> Int
--                 -> (Vector Double, Double) -- result at time n+steps
-- rk_multiStep fs xst h 1 = rk_1S fs xst h
-- rk_multiStep fs xst h steps = rk_multiStep fs (rk_1S fs xst h) h (steps - 1)