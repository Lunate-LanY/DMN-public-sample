{-# LANGUAGE Arrows
#-}
module DMN.Control.MainSF(
    mainSF
)  where

import DMN.Control.AuxSF
import DMN.Utils.Initial
import Data.Vector as V
import FRP.Yampa

-- generation of input sequence 
input_generation :: SF () Bool
input_generation = proc () -> do
    seq1 <- constant False -< ()
    seq2 <- delay 0.5 True -< seq1
    seq3 <- delay 2 False -< seq2
    returnA -< seq3

-- main signal function of simulation 
mainSF :: (RandomGen g) => SF (g, Double) (Vector Double, Vector (Event Int))
mainSF = proc (g,t) -> do
    -- resVect_now_prim, primary results at time (t+1) without taking firing into account
    -- resVect_now, final results at time (t+1)
    -- fireVect_now, firing vector at time (t+1)
    -- resVect_pre, results at time t
    -- fireVect_pre, firing vector at time t
    -- inp, indicate whether input exists
    -- t, time
    -- g, random number generator
    inp <- input_generation -< ()
    rec 
        resVect_pre <- iPre resVect_initial -< resVect_now  
        fireVect_pre <- iPre fireVect_initial -< fireVect_now
        resVect_now_prim <- oneStepCalc -< (resVect_pre, fireVect_pre, inp, t)
        (resVect_now, fireVect_now) <- updateResultFire -< (g, resVect_now_prim, fireVect_pre)
    returnA -< (resVect_now,fireVect_now)