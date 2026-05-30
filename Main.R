
# For each pi in pi_grid, generate R datasets, fit all five
# estimators on each, and store the coefficients.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("dgp.R")
source("fits.R")

run_mc <- function(pi_grid = c(0, 0.05, 0.1, 0.2, 0.5, 1.0),
                   R       = 500,
                   N       = 500,
                   T       = 10,
                   seed    = 8008,
                   verbose = TRUE) {
  
  set.seed(seed)
  
  n_coefs   <- 20                       # 5 estimators x 4 params
  n_rows    <- length(pi_grid) * R * n_coefs
  results   <- data.frame(
    pi        = numeric(n_rows),
    rep       = integer(n_rows),
    estimator = character(n_rows),
    parameter = character(n_rows),
    estimate  = numeric(n_rows),
    stringsAsFactors = FALSE
  )
  
  coef_names <- c(
    "pols.beta_u","pols.beta_c","pols.gamma_u","pols.gamma_c",
    "re.beta_u","re.beta_c","re.gamma_u","re.gamma_c",
    "fe.beta_u","fe.beta_c","fe.gamma_u","fe.gamma_c",
    "cre.beta_u","cre.beta_c","cre.gamma_u","cre.gamma_c",
    "ht.beta_u","ht.beta_c","ht.gamma_u","ht.gamma_c"
  )
  estimators <- sub("\\..*", "", coef_names)
  parameters <- sub(".*\\.", "", coef_names)
  
  row_idx <- 1L
  t_start <- Sys.time()
  
  for (pi in pi_grid) {
    if (verbose) cat(sprintf("\n--- pi = %.3f ---\n", pi))
    
    for (r in 1:R) {
      d  <- dgp(N = N, T = T, pi = pi)
      est <- tryCatch(
        fit_all(d),
        error = function(e) {
          if (verbose) cat(sprintf("  rep %d failed: %s\n", r, conditionMessage(e)))
          rep(NA_real_, n_coefs)
        }
      )
      
      idx <- row_idx:(row_idx + n_coefs - 1L)
      results$pi[idx]        <- pi
      results$rep[idx]       <- r
      results$estimator[idx] <- estimators
      results$parameter[idx] <- parameters
      results$estimate[idx]  <- est
      row_idx <- row_idx + n_coefs
      
      if (verbose && r %% 50 == 0) {
        elapsed <- as.numeric(Sys.time() - t_start, units = "secs")
        cat(sprintf("  rep %d / %d   elapsed %.1fs\n", r, R, elapsed))
      }
    }
  }
  
  t_total <- as.numeric(Sys.time() - t_start, units = "secs")
  if (verbose) cat(sprintf("\nDone. Total time: %.1fs (%.1f min)\n",
                           t_total, t_total / 60))
  
  results
}

# Summarize: bias and RMSE per (pi, estimator, parameter)
# True parameter values are 1 for all four parameters by default.

summarize_mc <- function(results,
                         truth = c(beta_u = 1, beta_c = 1,
                                   gamma_u = 1, gamma_c = 1)) {
  results$true_val <- truth[results$parameter]
  agg <- aggregate(
    cbind(estimate, true_val) ~ pi + estimator + parameter,
    data = results,
    FUN  = function(x) c(mean = mean(x, na.rm = TRUE),
                         n    = sum(!is.na(x)))
  )
  est_mean  <- agg$estimate[, "mean"]
  true_val  <- agg$true_val[, "mean"]   
  n_valid   <- agg$estimate[, "n"]

  rmse <- tapply(
    (results$estimate - results$true_val)^2,
    list(results$pi, results$estimator, results$parameter),
    FUN = function(x) sqrt(mean(x, na.rm = TRUE))
  )

  rmse_df <- as.data.frame.table(rmse, responseName = "rmse",
                                 stringsAsFactors = FALSE)
  names(rmse_df) <- c("pi","estimator","parameter","rmse")
  rmse_df$pi <- as.numeric(rmse_df$pi)
  
  # Bias data.frame
  bias_df <- data.frame(
    pi        = agg$pi,
    estimator = agg$estimator,
    parameter = agg$parameter,
    mean_est  = est_mean,
    bias      = est_mean - true_val,
    n_valid   = n_valid,
    stringsAsFactors = FALSE
  )
  

  out <- merge(bias_df, rmse_df,
               by = c("pi","estimator","parameter"),
               all.x = TRUE)
  out <- out[order(out$parameter, out$estimator, out$pi), ]
  rownames(out) <- NULL
  out
}

results <- run_mc(pi_grid = c(0, 0.05, 0.1, 0.2, 0.5, 1.0),
                  R       = 500)
summary_full <- summarize_mc(results)
saveRDS(results,     "mc_results_raw.rds")
saveRDS(summary_full, "mc_summary.rds")

# 1. gamma_c (the parameter we care most about)
cat("\n=== gamma_c (the main story) ===\n")
gc <- summary_full[summary_full$parameter == "gamma_c", ]
gc <- gc[order(gc$estimator, gc$pi), ]
print(gc, row.names = FALSE)

# 2. beta_c (endogenous time-varying — FE/CRE/HT should win)
cat("\n=== beta_c (endogenous time-varying) ===\n")
bc <- summary_full[summary_full$parameter == "beta_c", ]
bc <- bc[order(bc$estimator, bc$pi), ]
print(bc, row.names = FALSE)

# 3. beta_u (exogenous time-varying — all should be ~unbiased)
cat("\n=== beta_u (exogenous time-varying) ===\n")
bu <- summary_full[summary_full$parameter == "beta_u", ]
bu <- bu[order(bu$estimator, bu$pi), ]
print(bu, row.names = FALSE)

# 4. gamma_u exogenous time-invariant 
cat("\n=== gamma_u (exogenous time-invariant) ===\n")
gu <- summary_full[summary_full$parameter == "gamma_u", ]
gu <- gu[order(gu$estimator, gu$pi), ]
print(gu, row.names = FALSE)

