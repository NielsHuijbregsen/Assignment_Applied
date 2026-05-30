# ============================================================
# Hausman-Taylor Simulation Study
# Purpose: Fit all five estimators on one simulated dataset
# ============================================================
#
# Estimators:
#   POLS : pooled OLS
#   RE   : random effects
#   FE   : fixed effects (within)
#   CRE  : correlated random effects (Mundlak)
#   HT   : Hausman-Taylor
#
# Parameters of interest: beta_u, beta_c, gamma_u, gamma_c
#
# FE cannot estimate gamma_u or gamma_c -> reported as NA
#
# Returns a named numeric vector of length 20:
#   pols.beta_u, pols.beta_c, pols.gamma_u, pols.gamma_c,
#   re.beta_u,   re.beta_c,   re.gamma_u,   re.gamma_c,
#   fe.beta_u,   fe.beta_c,   fe.gamma_u,   fe.gamma_c,
#   cre.beta_u,  cre.beta_c,  cre.gamma_u,  cre.gamma_c,
#   ht.beta_u,   ht.beta_c,   ht.gamma_u,   ht.gamma_c

# Required packages: plm
# install.packages("plm")  # if not already installed
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(plm)

# Helper to extract a coefficient by name, returning NA if absent
get_coef <- function(model, name) {
  cf <- tryCatch(coef(model), error = function(e) NULL)
  if (is.null(cf)) return(NA_real_)
  if (name %in% names(cf)) cf[[name]] else NA_real_
}

fit_all <- function(d) {
  pd <- pdata.frame(d, index = c("id", "time"))
  
  #POLS
  m_pols <- plm(y ~ x_u + x_c + z_u + z_c,
                data = pd, model = "pooling")
  # RE
  m_re   <- plm(y ~ x_u + x_c + z_u + z_c,
                data = pd, model = "random")
  # FE 
  # FE drops z_u and z_c automatically (time-invariant)
  m_fe   <- plm(y ~ x_u + x_c + z_u + z_c,
                data = pd, model = "within")
  
  # CRE
  pd$xbar_u <- ave(as.numeric(pd$x_u), pd$id, FUN = mean)
  pd$xbar_c <- ave(as.numeric(pd$x_c), pd$id, FUN = mean)
  
  m_cre  <- plm(y ~ x_u + x_c + z_u + z_c + xbar_u + xbar_c,
                data = pd, model = "random")
  
  # Hausman-Taylor
  m_ht   <- pht(y ~ x_u + x_c + z_u + z_c | x_u + z_u,
                data = pd)
  out <- c(
    pols.beta_u  = get_coef(m_pols, "x_u"),
    pols.beta_c  = get_coef(m_pols, "x_c"),
    pols.gamma_u = get_coef(m_pols, "z_u"),
    pols.gamma_c = get_coef(m_pols, "z_c"),
    
    re.beta_u    = get_coef(m_re,   "x_u"),
    re.beta_c    = get_coef(m_re,   "x_c"),
    re.gamma_u   = get_coef(m_re,   "z_u"),
    re.gamma_c   = get_coef(m_re,   "z_c"),
    
    fe.beta_u    = get_coef(m_fe,   "x_u"),
    fe.beta_c    = get_coef(m_fe,   "x_c"),
    fe.gamma_u   = NA_real_,   # FE cannot estimate
    fe.gamma_c   = NA_real_,   # FE cannot estimate
    
    cre.beta_u   = get_coef(m_cre,  "x_u"),
    cre.beta_c   = get_coef(m_cre,  "x_c"),
    cre.gamma_u  = get_coef(m_cre,  "z_u"),
    cre.gamma_c  = get_coef(m_cre,  "z_c"),
    
    ht.beta_u    = get_coef(m_ht,   "x_u"),
    ht.beta_c    = get_coef(m_ht,   "x_c"),
    ht.gamma_u   = get_coef(m_ht,   "z_u"),
    ht.gamma_c   = get_coef(m_ht,   "z_c")
  )
  
  out
}