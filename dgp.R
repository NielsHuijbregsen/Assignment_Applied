
#Data-generating process for the panel data model

# Model:
#   y_it = beta_u * x_u_it + beta_c * x_c_it
#        + gamma_u * z_u_i  + gamma_c * z_c_i
#        + c_i + u_it
#
# where:
#   x_u_it : time-varying, exogenous       (uncorrelated with c_i)
#   x_c_it : time-varying, endogenous      (correlated with c_i)
#   z_u_i  : time-invariant, exogenous     (uncorrelated with c_i)
#   z_c_i  : time-invariant, endogenous    (correlated with c_i)
#   c_i    : unobserved individual effect
#   u_it   : idiosyncratic error
#
# Key DGP equation for the HT instrument:
#   z_c_i = pi * xbar_u_i + rho_z * c_i + r_i
#
# pi    controls the relevance  of the HT instrument xbar_u_i
# rho_z controls the endogeneity of z_c_i (fixed at 0.7)

dgp <- function(N        = 500,    # number of individuals
                T        = 10,     # time periods
                pi       = 0.5,    # relevance of HT instrument
                rho_z    = 0.7,    # endogeneity of z_c
                beta_u   = 1,      # true coef on x_u
                beta_c   = 1,      # true coef on x_c
                gamma_u  = 1,      # true coef on z_u
                gamma_c  = 1,      # true coef on z_c
                sigma_c  = 1,      # sd of c_i
                sigma_u  = 1,      # sd of u_it
                sigma_r  = 1,      # sd of r_i in z_c equation
                lambda   = 0.5     # correlation of x_c with c_i
) {
  
  #  Individual-level (time-invariant) draws 
  c_i      <- rnorm(N, mean = 0, sd = sigma_c)        # unobserved effect
  z_u_i    <- rnorm(N, mean = 0, sd = 1)              # exogenous TI regressor
  
  # time-varying regressors
  eta_u_i  <- rnorm(N, mean = 0, sd = 1)              
  eta_c_i  <- rnorm(N, mean = 0, sd = 1)            
  
  # Residuals
  r_i      <- rnorm(N, mean = 0, sd = sigma_r)
  
  #Time-varying regressors 
  id       <- rep(1:N, each = T)
  time     <- rep(1:T, times = N)
  
  # x_u_it: exogenous, independent of c_i
  nu_u     <- rnorm(N * T, mean = 0, sd = 1)
  x_u_it   <- eta_u_i[id] + nu_u
  
  # x_c_it: endogenous, correlated with c_i via lambda
  nu_c     <- rnorm(N * T, mean = 0, sd = 1)
  x_c_it   <- lambda * c_i[id] + eta_c_i[id] + nu_c
  
  # Individual mean of x_u
  # xbar_u_i = (1/T) * sum_t x_u_it
  xbar_u_i <- tapply(x_u_it, id, mean)
  xbar_u_i <- as.numeric(xbar_u_i[as.character(1:N)])
  
  # Endogenous time-invariant regressor z_c 
  z_c_i    <- pi * xbar_u_i + rho_z * c_i + r_i
  
  # Idiosyncratic error and outcome
  u_it     <- rnorm(N * T, mean = 0, sd = sigma_u)
  
  y_it <- beta_u  * x_u_it +
    beta_c  * x_c_it +
    gamma_u * z_u_i[id] +
    gamma_c * z_c_i[id] +
    c_i[id] +
    u_it

  data.frame(
    id   = id,
    time = time,
    y    = y_it,
    x_u  = x_u_it,
    x_c  = x_c_it,
    z_u  = z_u_i[id],
    z_c  = z_c_i[id]
  )
}
