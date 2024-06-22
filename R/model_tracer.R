model_tracer <- function(obs, Cin, n_run = 1, crit = c("KGE", "NSE", "KGENP", "KGEABS", "RMSE"),
                         eval = "all", allow_NA = FALSE,
                         warmup_time = 500, warmup_method = c("u_Cin", "u_Cobs", "1_Cin", "1_Cobs"),
                         max_t, type = c("EMM", "EPM", "PEM", "DM", "custom"),
                         MC = NULL, n_component = c(1, 2, 3),
                         p1_min, p1_max, p2_min, p2_max, ratio_min = NULL, ratio_max = NULL) {

  # Get number of model components
  n_component <- n_component[1]

  # Assign default ratio between 0 and 1
  if (is.null(ratio_min) | is.null(ratio_max)) {
    ratio_min <- rep(0, n_component)
    ratio_max <- rep(1, n_component)
  }

  # Check errors
  if ("custom" %in% type & is.null(MC)) stop("MC is set to custom but values are missing.")
  if (n_component < 1 | n_component > 4) stop("Number of component should be between 1 and 4")
  if (length(type) != n_component) stop("Number of component is not equal to number of type")
  if (length(p1_min) != n_component) stop("Number of component is not equal to number of p1_min")
  if (length(p1_max) != n_component) stop("Number of component is not equal to number of p1_max")
  if (length(p2_min) != n_component) stop("Number of component is not equal to number of p2_min")
  if (length(p2_max) != n_component) stop("Number of component is not equal to number of p2_max")
  if (length(ratio_min) != n_component) stop("Number of component is not equal to number of ratio_min")
  if (length(ratio_max) != n_component) stop("Number of component is not equal to number of ratio_max")

  # Length of obs
  l_obs <- length(obs)

  # Optimisation part -- "all" is default and return the whole time series
  if (eval[1] == "all") eval_r <- 1:l_obs else eval_r <- eval[1]:eval[2]

  # Get MC function
  if (n_component == 1) type <- match.arg(type)

  # Get objective function
  crit <- match.arg(crit)

  # Get objective function
  warmup_method <- match.arg(warmup_method)

  # Add warmup values
  if (warmup_method == "u_Cin") warmup_val <- rep(mean(Cin, na.rm = TRUE), warmup_time)
  if (warmup_method == "u_Cobs") warmup_val <- rep(mean(obs, na.rm = TRUE), warmup_time)
  if (warmup_method == "1_Cin") warmup_val <- rep(Cin[1], warmup_time)
  if (warmup_method == "1_Cobs") warmup_val <- rep(obs[1], warmup_time)

  Cin_warmup <- c(warmup_val, Cin)

  # Initialise output
  all <- list("obj" = rep(NA_real_, n_run),
              "p1" = list(),
              "p2" = list(),
              "ratio" = list(),
              "sim" = vector(mode = "list", length = n_run))

  for (n in seq(n_component)) {
    all$p1[[type[n]]] <- rep(NA_real_, n_run)
    all$p2[[type[n]]] <- rep(NA_real_, n_run)
    all$ratio[[type[n]]] <- rep(NA_real_, n_run)
  }

  best <- list("obs" = obs,
               "Cin" = Cin,
               "sim" = numeric(l_obs),
               "obj" = NA_real_,
               "obj_name" = crit,
               "p1" = list(),
               "p2" = list(),
               "ratio" = list())

  for (c in seq(n_component)) {
    best$p1[[type[c]]] <- NA_real_
    best$p2[[type[c]]] <- NA_real_
    best$ratio[[type[c]]] <- NA_real_
  }

  # Sample parameters
  for (c in seq(n_component)) {
    all$p1[[type[c]]] <- stats::runif(n_run, p1_min[c], p1_max[c])
    all$p2[[type[c]]] <- stats::runif(n_run, p2_min[c], p2_max[c])
    all$ratio[[type[c]]] <- stats::runif(n_run, ratio_min[c], ratio_max[c])
  }

  # Normalise ratio to 1
  ratio_sum <- Reduce(`+`, all$ratio)
  for (c in seq(n_component)) {
    all$ratio[[type[c]]] <- all$ratio[[type[c]]] / ratio_sum
  }

  # Loop ------------------------------------------------------
  for (n in seq(1, n_run)) {

    # Calculate MC
    if (is.na(max_t)) max_t <- 1
    sim_list <- list()

    for (c in seq(n_component)) {
      MC <- do.call(paste0("MC_", type[c]), list(all$p1[[type[c]]][n], all$p2[[type[c]]][n], max_t))
      sim_list[[c]] <- rev(stats::convolve(rev(MC), Cin_warmup, conj = TRUE, type = "open"))
      sim_list[[c]] <- sim_list[[c]][(warmup_time + 1):(warmup_time + l_obs)] * all$ratio[[type[c]]][n]
    }

    sim <- Reduce(`+`, sim_list)

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
      best$p1 <- lapply(all$p1, `[`, n)
      best$p2 <- lapply(all$p2, `[`, n)
      best$ratio <- lapply(all$ratio, `[`, n)
      message(paste0(crit, ": ", round(best$obj, 5), " ––– ",
                     paste(paste0(lapply(best$ratio, "round", 3), " * ",
                           type, "[", lapply(best$p1, "round", 2),
                           "; ", lapply(best$p2, "round", 2),
                           "]"),
                    collapse = " + ")))
    }
  }

  return(list("best" = best, "all" = all))
}
