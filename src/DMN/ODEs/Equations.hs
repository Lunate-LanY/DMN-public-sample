module DMN.ODEs.Equations(
    uP_i_n, uIa_i_n, uIb_i_n, uGl_i_n,
    uPDMN_i, uIbDMN_i,
    rP_j_n, rIa_j_n, rIb_j_n, rext_i_n,
    rPDMN_i, rIbDMN_i,
    gabaP_ext_i_n
) where

import qualified Data.Vector as V
import DMN.Utils.Constants
import DMN.Utils.Structure

{-
    ODE Vector: length = 1520
        - u^P_i(n;t)          <-> n * 20 + i
        - u^Ia_i(n;t)         <-> 160 + n * 20 + i
        - u^Ib_i(n;t)         <-> 320 + n * 20 + i
        - u^P{DMN}_i(t)       <-> 480 + i
        - u^Ib{DMN}_i(t)      <-> 500 + i
        - u^Gl_i(n;t)         <-> 520 + n * 20 + i
        - r^P_j(n;t)          <-> 680 + n * 20 + i
        - r^Ia_j(n;t)         <-> 840 + n * 20 + i
        - r^Ib_j(n;t)         <-> 1000 + n * 20 + i
        - r^P{DMN}_i(t)       <-> 1160 + i
        - r^Ib{DMN}_i(t)      <-> 1180 + i
        - r^P_i,ext(n;t)      <-> 1200 + n * 20 + i
        - [GABA]^P_i,ext(n;t) <-> 1360 + n * 20 + i
-}

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
-- PART 1: ODE for Potentials
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

-- ODE for u^P_i(n;t)
uP_i_n :: Int  -- stands for the order of cell assembly, i.e. `n`
       -> Int  -- stands for the order of this P cell in this cell assembly, i.e. `i`
       -> Bool    -- stands for whether there are sensory input
       -> V.Vector Double -> Double ->  Double  -- result ODE
uP_i_n n i p vect t = cdu_dt / c_m_P
    where
        -- self: u^P_i(n;t)
        self = vect V.! (uP_start_pos + n * 20 + i)

        -- wr_P2P = \Sigma_{j = 1(j \neq i)}^N w_{ij}^{P,P} * r_i^P(n;t)
        wr_P2P = let f acc j = if (j /= i) 
                               then (acc + w_P2P * (vect V.! (rP_start_pos + 20 * n + j)))
                               else acc
                 in foldl f 0 [0..19]
        -- I_i^{P,Ib}(n;t)
        i_PP = - g_hat_AMPA * (self - u_rev_AMPA) * wr_P2P

        -- wr_Ib2P = \Sigma_{j=1}^N w_{ij}^{P,Ib} * r_j^{Ib}(n;t)
        wr_Ib2P = foldl (\acc j -> acc + w_Ib2P * (vect V.! (rIb_start_pos + n * 20 + j))) 0 [0..19]
        -- I_i^{P,Ib}(n;t)
        i_PIb = - g_hat_GABA * (self - u_rev_GABA) * wr_Ib2P

        -- I_{i,ext}^P(n;t)
        i_Pext = - g_hat_GABA * (self - u_rev_GABA) * delta_P * (vect V.! (rP_ext_start_pos + n * 20 + i))
    
        -- inp
        inp = 3
        -- I_{inp}^P(n;t)
        i_Pinp = if p
                 then alpha_P * (exp $ negate $ ((fromIntegral (n - inp)) / tau_P) ** 2) 
                 else 0
        -- c^P_m * \frac{du_i^P(n;t)}{dt}
        cdu_dt = - g_m_P * (self - u_rest_P) + i_PP + i_PIb + i_Pext + i_Pinp
    

-- ODE for u^{Ia}_i(n;t)
uIa_i_n :: Int -- stands for the order of cell assembly, i.e. `n`
        -> Int -- stands for the order of this Ia cell in this cell assembly, i.e. `i`
        -> V.Vector Double -> Double -> Double -- result ODE
uIa_i_n n i vect t = cdu_dt / c_m_Ia
    where
        -- u_i^{Ia}(n;t)
        self = vect V.! (uIa_start_pos + n * 20 + i)

        -- I_i^{Ia,P}(n;t)
        i_IaP = - g_hat_AMPA * (self - u_rev_AMPA) * w_P2Ia * (vect V.! (rP_start_pos + n * 20 + i))

        -- wr_PDMN2Ia = \Sigma_{j=1}^N w_{ij}^{Ia,P_DMN} * r_j^{P_DMN}(t)
        wr_PDMN2Ia = foldl (\acc j -> acc + w_PDMN2Ia * (vect V.! (rP_DMN_start_pos + j))) 0 [0..19]
        -- I_i^{Ia,P_DMN}(n;t)
        i_IaPDMN = - g_hat_AMPA * (self - u_rev_AMPA) * wr_PDMN2Ia

        -- c_m^{Ia} * \frac{du_i^{Ia}(n;t)}{dt}
        cdu_dt = - g_m_Ia * (self - u_rest_Ia) + i_IaP + i_IaPDMN

-- ODE for u^{Ib}_i(n;t)
uIb_i_n :: Int -- stands for the order of cell assembly, i.e. `n`
        -> Int -- stands for the order of this Ib cell in this cell assembly, i.e. `i`
        -> V.Vector Double -> Double -> Double  -- result ODE
uIb_i_n n i vect t = cdu_dt / c_m_Ib
    where
        -- u_i^{Ib}(n;t)
        self = vect V.! (uIb_start_pos + n * 20 + i)
        
        -- wr_P2Ib = \Sigma_{n' = 0(n' \neq n)}^M w_i^{Ib,P}(n,n') * r_i^P(n',t) 
        wr_P2Ib = let f acc n' = if (n' /= n)
                                 then acc + w_P2Ib_diff * (vect V.! (rP_start_pos + n' * 20 + i))
                                 else acc
                  in foldl f 0 [0..7]
        -- I_i^{Ib,P}(n;t)
        i_IbP = - g_hat_AMPA * (self - u_rev_AMPA) * wr_P2Ib

        -- c_m^{Ib} * \frac{du_i^{Ib}(n;t)}{dt}
        cdu_dt = - g_m_Ib * (self - u_rest_Ib) + i_IbP

-- ODE for u^{Gl}_i(n;t)
uGl_i_n :: Int -- stands for the order of cell assembly, i.e. `n`
        -> Int -- stands for the order of this Glial cell in this cell assembly, i.e. `i`
        -> V.Vector Double -> Double -> Double  -- result ODE
uGl_i_n n i vect t = cdu_dt / c_m_Gl
    where
        -- u^{Gl}_i(n;t)
        self = vect V.! (uGl_start_pos + n * 20 + i)

        -- I_i^{Gl,Ia}(n;t)
        i_GlIa = - g_hat_GABA * (self - u_rev_GABA) * w_Ia2Gl * (vect V.! (rIa_start_pos + n * 20 + i))

        -- c_m^{Gl} * \frac{du_i^{Gl}(n;t)}{dt}
        cdu_dt = - g_m_Gl * (self - u_rest_Gl) + i_GlIa

-- ODE for u_i^{P_DMN}(t)
uPDMN_i :: Int -- stands for the order of this P cell in DMN, i.e. `i` 
                  -- (0 .. 19)
        -> V.Vector Double -> Double -> Double -- result ODE
uPDMN_i i vect t = cdu_dt / c_m_P
    where 
        -- u_i^{P_DMN}(t)
        self = vect V.! (uP_DMN_start_pos + i)

        -- synaptics from all Ib_DMN cells to the current P_DMN
        wr_IbDMN2PDMN = foldl (\acc j -> acc + w_IbDMN2PDMN * (vect V.! (rIb_DMN_start_pos + j))) 0 [0..19]
        
        -- I_i^{P_DMN,Ib_DMN}(t)
        i_PDMN_IbDMN = - g_hat_GABA * (self - u_rev_GABA) * wr_IbDMN2PDMN
        
        -- synaptics from all P_DMN cells (except itself) to the current P_DMN
        wr_PDMN2PDMN = foldl (\acc j -> if (i /= j)
                                        then acc + w_PDMN2PDMN * (vect V.! (rP_DMN_start_pos + j))
                                        else acc) 0 [0..19]
        -- I_i^{P_DMN,P_DMN}(t)
        i_PDMN_PDMN = negate $ g_hat_AMPA * (self - u_rev_AMPA) * wr_PDMN2PDMN

        -- c^P_m * \frac{du_i^{P_DMN}(t)}{dt}
        cdu_dt = - g_m_P * (self - u_rest_P) + i_PDMN_IbDMN + i_PDMN_PDMN

-- ODE for u_i^{Ib_DMN}(t)
uIbDMN_i :: Int -- stands for the order of this Ib cell in DMN, i.e. `i` (0..19)
         -> V.Vector Double -> Double -> Double -- result ODE
uIbDMN_i i vect t = cdu_dt /c_m_Ib
    where
        -- u_i^{Ib_DMN}(t)
        self = vect V.! (uIb_DMN_start_pos + i)

        -- wr_P2IbDMN = \Sigma_{n = 0}^M \Sigma_{j = 1}^N w_j^{Ib_DMN,P}(n) * r_j^P(n;t)
        wr_P2IbDMN = let f n j = w_P2IbDMN * (vect V.! (rP_start_pos + n * 20 + j) )
                     in sum $ f <$> [0..7] <*> [0..19]
        -- I_i^{Ib_DMN,P}(t)
        i_IbDMN_P = - g_hat_AMPA * (self - u_rev_AMPA) * wr_P2IbDMN

        cdu_dt = - g_m_Ib * (self - u_rest_Ib) + i_IbDMN_P


--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
-- PART 2: ODEs for Receptor Dynamics
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

-- ODE for r_j^P(n;t)
rP_j_n :: Int  -- stands for the order of cell assembly, i.e. `n`
       -> Int  -- stands for the order of this P cell in this cell assembly, i.e. `j`
       -> Bool    -- stands for whether the corresponding P cell are firing now
       -> V.Vector Double -> Double -> Double  -- result ODE
rP_j_n n j isFiring vect t = alpha_AMPA * glut * (1 - self) - beta_AMPA * self
    where
        -- r_j^P(n;t)
        self = vect V.! (rP_start_pos + n * 20 + j)

        -- [Glut]_j(n;t)
        glut = if isFiring then 1e-3 else 0


-- ODE for r_j^Ia(n;t)
rIa_j_n :: Int  -- stands for the order of cell assembly, i.e. `n`
        -> Int  -- stands for the order of this Ia cell in this cell assembly, i.e. `j`
        -> Bool    -- stands for whether the corresponding Ia cell are firing now
        -> V.Vector Double -> Double -> Double  -- result ODE
rIa_j_n n j isFiring vect t = alpha_GABA * gaba * (1 - self) - beta_GABA * self
    where
        -- r_j^Ia(n;t)
        self = vect V.! (rIa_start_pos + n * 20 + j)

        -- [GABA]_j^{Ia}(n;t)
        gaba = if isFiring then 1e-3 else 0

-- ODE for r_j^Ib(n;t)
rIb_j_n :: Int  -- stands for the order of cell assembly, i.e. `n`
        -> Int  -- stands for the order of this Ib cell in this cell assembly, i.e. `j`
        -> Bool    -- stands for whether the corresponding Ib cell are firing now
        -> V.Vector Double -> Double -> Double -- result ODE
rIb_j_n n j isFiring vect t = alpha_GABA * gaba * (1 - self) - beta_GABA * self
    where
        -- r_j^Ib(n;t)
        self = vect V.! (rIb_start_pos + n * 20 + j)

        -- [GABA]_j^{Ib}(n;t)
        gaba = if isFiring then 1e-3 else 0

-- ODE for r_{i,ext}^P(n;t)
rext_i_n :: Int  -- stands for the order of cell assembly, i.e. `n`
         -> Int  -- stands for the order of this unit in this cell assembly, i.e. `i`
         -> V.Vector Double -> Double -> Double
rext_i_n n i vect t = alpha_GABA * gaba_ext * (1 - self) - beta_GABA * self
    where
        -- r_{i,ext}^P(n;t)
        self = vect V.! (rP_ext_start_pos + n * 20 + i)

        -- [GABA]_{i,ext}^P(n;t)
        gaba_ext = vect V.! (gaba_ext_start_pos + n * 20 + i)

-- ODE for r_i^{P_DMN}(t)
rPDMN_i :: Int  -- stands for the order of P cell in DMN, i.e. `i` (0..39)
       -> Bool -- stands for whether the corresponding P cell are firing now
       -> V.Vector Double -> Double -> Double  -- result ODE
rPDMN_i i isFiring vect t = alpha_AMPA * glut_DMN * (1 - self) - beta_AMPA * self
    where
        -- r_i^{P_DMN}(t)  
        self = vect V.! (rP_DMN_start_pos + i)

        -- [Glut]_i^{DMN}(t)
        glut_DMN = if isFiring then 1e-3 else 0

-- ODE for r_i^{Ib_DMN}(t)
rIbDMN_i :: Int  -- stands for the order of Ib cell in DMN, i.e. `i` (0..19)
         -> Bool -- stands for whether the corresponding Ib cell are firing now
         -> V.Vector Double -> Double -> Double  -- result ODE
rIbDMN_i i isFiring vect t = alpha_GABA * gaba_DMN * (1 - self) - beta_GABA * self
    where
        -- r_i^{Ib_DMN}(t)
        self = vect V.! (rIb_DMN_start_pos + i)

        -- [GABA]_i^{DMN}(t)
        gaba_DMN = if isFiring then 1e-3 else 0

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
-- PART 3: ODEs for Ambient GABA
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
-- [GABA]_{i,ext}^P(n;t)
gabaP_ext_i_n :: Int  -- stands for the order of cell assembly, i.e. `n`
              -> Int  -- stands for the order of the corresponding P cell in this cell assembly, i.e. `i`
              -> V.Vector Double -> Double -> Double -- result ODE
gabaP_ext_i_n n i vect t = (negate gamma_trn) * (self - gaba_ext_0) + 
                         t_Gl * (gaba_max - self) * (self - gaba_min) *
                         (uGl - u_rev_Gl)
    where 
        -- [GABA]_{i,ext}^P(n;t)
        self = vect V.! (gaba_ext_start_pos + n * 20 + i)

        -- u_i^{Gl}(n;t)
        uGl = vect V.! (uGl_start_pos + n * 20 + i)