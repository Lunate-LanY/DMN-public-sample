{-# LANGUAGE Strict
#-}
module DMN.Utils.Initial(
    resVect_initial,
    fireVect_initial
) where

import qualified Data.Vector as V
import FRP.Yampa (Event(..))
import DMN.Utils.Constants

-- initial result vectors, for specific info, see DMN.ODEs.Equations
resVect_initial :: V.Vector Double
resVect_initial = V.fromList $ replicate 160 u_rest_P ++ (
                                 replicate 160 u_rest_Ia ++ (
                                     replicate 160 u_rest_Ib ++ (
                                       replicate 20 u_rest_P++ (
                                          replicate 20 u_rest_Ib ++ (
                                            replicate 160 u_rest_Gl ++ (
                                                 replicate 680 0 ++ 
                                                     replicate 160 gaba_ext_0
                                             )
                                         )
                                       )
                                    )
                                 )
                               )

-- initial firing vector
fireVect_initial :: V.Vector (Event Int)
fireVect_initial = V.replicate 520 NoEvent