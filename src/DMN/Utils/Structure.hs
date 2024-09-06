module DMN.Utils.Structure(
  uP_start_pos, uIa_start_pos, uIb_start_pos, uP_DMN_start_pos, uIb_DMN_start_pos, uGl_start_pos, 
  rP_start_pos, rIa_start_pos, rIb_start_pos, rP_DMN_start_pos, rIb_DMN_start_pos, rP_ext_start_pos, gaba_ext_start_pos    
)  where

-- start position of all ODEs' variables in the result vector (and also in the firing vector)
uP_start_pos, uIa_start_pos, uIb_start_pos, uP_DMN_start_pos, uIb_DMN_start_pos, uGl_start_pos, rP_start_pos, rIa_start_pos,
  rIb_start_pos, rP_DMN_start_pos, rIb_DMN_start_pos, rP_ext_start_pos, gaba_ext_start_pos :: Int 

uP_start_pos = 0
uIa_start_pos = 160
uIb_start_pos = 320
uP_DMN_start_pos = 480
uIb_DMN_start_pos = 500

uGl_start_pos = 520

rP_start_pos = 680
rIa_start_pos = 840
rIb_start_pos = 1000
rP_DMN_start_pos = 1160
rIb_DMN_start_pos = 1180

rP_ext_start_pos = 1200
gaba_ext_start_pos = 1360
