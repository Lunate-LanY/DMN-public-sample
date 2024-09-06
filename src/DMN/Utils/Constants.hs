module DMN.Utils.Constants(
  c_m_Gl, c_m_Ia, c_m_Ib, c_m_P, 
  g_m_Gl, g_m_Ia, g_m_Ib, g_m_P, 
  u_rest_Gl, u_rest_Ia, u_rest_Ib, u_rest_P, 
  g_hat_AMPA, g_hat_GABA,
  u_rev_AMPA, u_rev_GABA, 
  u_rev_Gl, 
  w_Ia2Gl, w_Ib2P, w_IbDMN2PDMN, w_P2Ia, w_P2IbDMN, w_P2Ib_diff, w_P2P, w_PDMN2Ia, w_PDMN2PDMN,
  delta_P, 
  alpha_P, 
  tau_P, 
  alpha_AMPA, alpha_GABA, 
  beta_AMPA, beta_GABA, 
  eta_Ia, eta_Ib, eta_P, eta_P_DMN, eta_Ib_DMN,
  zeta_Ia, zeta_Ib, zeta_P, zeta_P_DMN, zeta_Ib_DMN,
  gamma_trn, gaba_ext_0, gaba_max, gaba_min, 
  t_Gl,
  u_fire
) where

c_m_Gl, c_m_Ia, c_m_Ib, c_m_P, g_m_Gl, g_m_Ia, g_m_Ib, g_m_P, u_rest_Gl, u_rest_Ia, u_rest_Ib, u_rest_P, g_hat_AMPA, g_hat_GABA,
  u_rev_AMPA, u_rev_GABA, u_rev_Gl, w_Ia2Gl, w_Ib2P, w_IbDMN2PDMN, w_P2Ia, w_P2IbDMN, w_P2Ib_diff, w_P2P, w_PDMN2Ia, w_PDMN2PDMN,
  delta_P, alpha_P, tau_P, alpha_AMPA, alpha_GABA, beta_AMPA, beta_GABA, eta_Ia, eta_Ib, eta_P, eta_P_DMN, eta_Ib_DMN, 
  zeta_Ia, zeta_Ib, zeta_P, zeta_P_DMN, zeta_Ib_DMN, gamma_trn, gaba_ext_0, gaba_max, gaba_min, t_Gl, u_fire :: Double
-- Membrane capacity of type K cells
-- nF
c_m_P = 0.5
c_m_Ia = 0.2
c_m_Ib = 0.6
c_m_Gl = 0.045

-- Membrane conductance
-- nS
g_m_P = 25
g_m_Ia = 20
g_m_Ib = 15
g_m_Gl = 9

-- Resting Potential
-- V
u_rest_P = -0.065
u_rest_Ia = -0.07
u_rest_Ib = -0.07
u_rest_Gl = -0.07

-- Maximum conductance for type Z receptor
-- nS
g_hat_AMPA = 0.5
g_hat_GABA = 0.7

-- Reversal potential
-- V
u_rev_AMPA = 0
u_rev_GABA = -0.08

-- synaptic weight from jth P cell to ith P cell
w_P2P = 6.5
-- synaptic weight from jth Ib cell to ith P cell
w_Ib2P = 4
-- synaptic weight from ith P to Ia cell
w_P2Ia = 30
-- synaptic weight from jth P cell of DMN to ith Ia cell of N_sen
-- w_PDMN2Ia = 0
w_PDMN2Ia = 4

-- synaptic weight from ith P to Ib cell between different cell assemblies
w_P2Ib_diff = 60
-- synaptic weight from ith Ia to glial cell
w_Ia2Gl = 20
-- synaptic weight from jth to ith P cell of DMN
w_PDMN2PDMN = 8
-- synaptic weight from jth Ib to ith P cell of DMN
w_IbDMN2PDMN = 14
-- synaptic weight from jth P cell of N_sen to ith Ib cell of DMN
w_P2IbDMN = 4.5

-- Amount of extrasynaptic GABA_a receptors on P cell
delta_P = 600

-- Sensory input current
-- nA
-- alpha_P = 0.29
alpha_P = 0.29

-- Broadness of sensory input
-- tau_P = 5
tau_P = 0.1

-- Channel openning rate for type Z receptors
-- M^{-1}sec^{-1}
alpha_AMPA = 1.1e6
alpha_GABA = 5e6

-- Channel closing rate
-- sec^{-1}
beta_AMPA = 190
beta_GABA = 180

-- Steepness of sigmoid function for type Y cell
eta_P = 270
eta_Ia = 240
eta_Ib = 360

eta_P_DMN = 230
eta_Ib_DMN = 320

-- Threshold of sigmoid function
-- V
zeta_P = -0.0335
zeta_Ia = -0.039
zeta_Ib = -0.04

zeta_P_DMN = -0.033
zeta_Ib_DMN = -0.036

-- Delay constant for ambient GABA concentration
gamma_trn = 2.5

-- M
-- Basal ambient GABA concentration
gaba_ext_0 = 1e-6
-- Maximum ambient GABA concentration
gaba_max = 1.5e-6
-- Minimal ambient GABA concentration
gaba_min = 0

-- GABA transfer coefficient
t_Gl = 1.4e9

-- Reversal potential of GABA transporter
-- V
u_rev_Gl = -0.07


-- membrane potential at firing state
-- V
u_fire = -0.01
