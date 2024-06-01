model_tracer <- function(obs, Cin, n_run = 1, crit = c("KGE", "NSE", "KGENP", "KGEABS", "RMSE"),
                         eval = "all", allow_NA = FALSE,
                         warmup_time = 500, warmup_method = c("u_Cin", "u_Cobs", "1_Cin", "1_Cobs"),
                         max_t, type = c("EMM", "EPM", "PEM", "DM", "custom"),
                         MC = NULL,
                         p1_min, p1_max, p2_min, p2_max) {

  # Check errors
  if (type == "custom" & is.null(MC)) stop("MC is set to custom but values are missing.")

  # Length of obs
  l_obs <- length(obs)

  # Optimisation part -- "all" is default and return the whole time series
  if (eval[1] == "all") eval_r <- 1:l_obs else eval_r <- eval[1]:eval[2]

  # Get MC function
  type <- match.arg(type)

  # Get objective function
  crit <- match.arg(crit)

  # Get objective function
  warmup_method <- match.arg(warmup_method)

  # Add warmup values
  if (warmup_method == "u_Cin") warmup_val <- rep(mean(Cin), warmup_time)
  if (warmup_method == "u_Cobs") warmup_val <- rep(mean(obs), warmup_time)
  if (warmup_method == "1_Cin") warmup_val <- rep(Cin[1], warmup_time)
  if (warmup_method == "1_Cobs") warmup_val <- rep(obs[1], warmup_time)

  Cin_warmup <- c(warmup_val, Cin)

  # Initialise output
  all <- list("obj" = rep(NA_real_, n_run),
              "p1" = rep(NA_real_, n_run),
              "p2" = rep(NA_real_, n_run),
              "sim" = vector(mode = "list", length = n_run))

  best <- list("obs" = obs,
               "Cin" = Cin,
               "sim" = numeric(l_obs),
               "obj" = NA_real_,
               "obj_name" = crit,
               "p1" = NA_real_,
               "p2" = NA_real_)

  # Sample parameters
  all$p1 <- stats::runif(n_run, p1_min, p1_max)
  all$p2 <- stats::runif(n_run, p2_min, p2_max)

  # Loop
  for (n in seq(1, n_run)) {
    # Calculate MC
    if (is.na(max_t)) max_t <- 1
    if (type == "EMM") MC <- MC_EMM(all$p1[n], max_t = max_t)
    if (type == "EPM") MC <- MC_EPM(all$p1[n], all$p2[n], max_t = max_t)
    if (type == "PEM") MC <- MC_PEM(all$p1[n], all$p2[n], max_t = max_t)
    if (type == "DM") MC <- MC_DM(all$p1[n], all$p2[n], max_t = max_t)

    # Convolution
    sim <- rev(stats::convolve(rev(MC), Cin_warmup, conj = TRUE, type = "open"))
    sim <- sim[(warmup_time + 1):(warmup_time + l_obs)]

    # Calculte score
    if (crit == "KGE") obj <- KGE(sim[eval_r], obs[eval_r], allow_NA = allow_NA)
    if (crit == "NSE") obj <- NSE(sim[eval_r], obs[eval_r], allow_NA = allow_NA)
    if (crit == "KGENP") obj <- KGENP(sim[eval_r], obs[eval_r], allow_NA = allow_NA)
    if (crit == "KGEABS") obj <- KGEABS(sim[eval_r], obs[eval_r], allow_NA = allow_NA)
    if (crit == "RMSE") obj <- RMSE(sim[eval_r], obs[eval_r], allow_NA = allow_NA)

    # Store all results
    all$obj[n] <- obj
    all$sim[[n]] <- sim

    # Check obj vs best (needed as RMSE is lower than)
    if (crit == "RMSE") check_obj <- obj < best$obj else check_obj <- obj > best$obj

    # Store best results
    if (!is.na(obj) & check_obj | is.na(best$obj)) {
      best$MC <- c(0, MC[MC >= max(MC / 100)])
      best$obj <- obj
      best$sim <- sim
      best$p1 <- all$p1[n]
      best$p2 <- all$p2[n]
      message(paste0(crit, ": ", round(best$obj, 5)))
    }
  }

  return(list("best" = best, "all" = all))
}
