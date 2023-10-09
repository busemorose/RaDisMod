model <- function(obs, P, n_run = 1, crit = c("KGE", "NSE", "KGENP", "KGEABS", "RMSE"), eval = "all", allow_NA = FALSE,
                  J_min, J_max,
                  max_t, type = c("socose", "gamma", "lnorm", "triangle", "custom"),
                  HU = NULL,
                  p1_min, p1_max, p2_min, p2_max) {

  # Check errors
  if (type == "custom" & is.null(HU)) stop("HU is set to custom but values are missing.")

  # Length of obs
  l_obs <- length(obs)

  # Optimisation part -- "all" is default and return the whole time series
  if (eval[1] == "all") eval_r <- 1:l_obs else eval_r <- eval[1]:eval[2]

  # Calculate P_cum
  P_cum <- cumsum(P)

  # Get HU function
  type <- match.arg(type)
  if (type != "custom") source(paste0("R/utils/HU_", type, ".R"))

  # Get SCS function
  source("R/utils/SCS.R")

  # Get objective function
  crit <- match.arg(crit)
  source(paste0("R/utils/", crit, ".R"))

  # Initialise output
  all <- list("obj" = rep(NA_real_, n_run),
              "J" = rep(NA_real_, n_run),
              "p1" = rep(NA_real_, n_run),
              "p2" = rep(NA_real_, n_run),
              "sim" = vector(mode = "list", length = n_run))

  best <- list("obs" = obs,
               "sim" = numeric(l_obs),
               "obj" = NA_real_,
               "obj_name" = crit,
               "J" = NA_real_,
               "p1" = NA_real_,
               "p2" = NA_real_)

  # Sample parameters
  all$J <- runif(n_run, J_min, J_max)
  all$p1 <- runif(n_run, p1_min, p1_max)
  all$p2 <- runif(n_run, p2_min, p2_max)

  # Loop
  for (n in seq(1, n_run)) {
    # Calculate HU
    if (type == "socose") HU <- HU_socose(all$p1[n], all$p2[n], max_t = max_t)
    if (type == "gamma") HU <- HU_gamma(all$p1[n], all$p2[n], max_t = max_t)
    if (type == "lnorm") HU <- HU_lnorm(all$p1[n], all$p2[n], max_t = max_t)
    if (type == "triangle") HU <- HU_triangle(all$p1[n], all$p2[n], max_t = max_t)

    # Net precipitation
    PN_cum <- SCS(P_cum, all$J[n])
    PN <-  diff(c(0, PN_cum))

    # Convolution
    sim <- rev(convolve(rev(HU), PN, conj = TRUE, type = "open"))
    sim <- sim[1:l_obs] # Same length as obs

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
      best$HU <- c(0, HU[HU >= max(HU / 100)])
      best$obj <- obj
      best$PN <- PN
      best$sim <- sim
      best$J <- all$J[n]
      best$p1 <- all$p1[n]
      best$p2 <- all$p2[n]
      message(paste0(crit, ": ", round(best$obj, 5)))
    }
  }

  return(list("best" = best, "all" = all))
}
