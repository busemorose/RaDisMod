#' Calculate performance criteria scores between a simulated and observed time series
#'
#' @param sim A numeric vector of simulated time series.
#' @param obs A numeric vector of observed time series.
#' @param crit A string vector of performance criteria to calculate.
#' @param sf A numeric vector for specifying scaling factors if needed for correlation, variability and bias, respectively. Default values are 1-1-1.
#' @param na.rm A boolean for na remove.
#' @param format A string vector for defining the output format, either vector or list.
#'
#' @return A numeric named vector or list of scores.
#' @export
#'
#' @examples
#' data(KarstMod_dataset)
#' obs <- KarstMod_dataset$QobsS
#' sim <- obs + runif(nrow(KarstMod_dataset), 0, sd(obs)) # Synthetic sim
#'
#' score(sim, obs, crit = c("NSE", "KGE", "KGE_m"), sf = c(2, 1, 1), na.rm = FALSE)

score <- function(sim,
                  obs,
                  crit = c("NSE", "BE", "C2M", "rpearson", "rspearman", "RMSE",
                           "KGE", "KGE_m", "KGE_m2", "KGENP", "LME", "LCE",
                           "KGE_abs", "KGE_m_abs", "KGENP_abs",
                           "bias", "MAE", "beta", "beta_abs", "alpha", "gamma", "beta_n", "CI"),
                  sf = c(1, 1, 1),
                  na.rm = FALSE,
                  allow_NA = FALSE,
                  format = c("vector", "list")) {
  
  # Get output format
  format <- match.arg(format)
  
  # Initiate output
  output <- list()
  
  # Check if sim and obs are numeric
  if(!is.numeric(sim) || !is.numeric(obs))
    stop("Invalid argument type: both sim and obs must be numeric.")
  
  # Check if sim and obs have the same length
  if(length(sim) != length(obs))
    stop("Invalid arguments: sim and obs must have the same length.")
  
  # Remove NA values if allowed
  if (allow_NA) {
    na <- is.na(obs)
    
    obs <- obs[!na]
    sim <- sim[!na]
  }
  
  # Calculate mean sim and obs
  mean_s = mean(sim, na.rm = na.rm)
  mean_o = mean(obs, na.rm = na.rm)
  
  # Calculate sd sim and obs
  sd_s = sd(sim, na.rm = na.rm)
  sd_o = sd(obs, na.rm = na.rm)
  
  # Calculate CV sim and obs
  CV_s <- sd_s / mean_s
  CV_o <- sd_o / mean_o
  
  # Calculate sum sim and obs
  sum_s = sum(sim, na.rm = na.rm)
  sum_o = sum(obs, na.rm = na.rm)
  
  # Calculate normalised flow duration curves
  fdc_s = sort(sim / (mean_s * length(sim)))
  fdc_o = sort(obs / (mean_o * length(obs)))
  
  # KGE components
  alpha <-  sd_s / sd_o
  gamma <- CV_s / CV_o
  alphaNP <- 1 - 0.5 * sum(abs(fdc_s - fdc_o))
  beta <- mean_s / mean_o
  beta_n <- (mean_s - mean_o) / sd_o
  
  # beta_abs and beta_ratio components
  dif <- sim - obs
  q_sum <- sum(obs)
  B_pos <- abs(sum(dif[dif > 0]))
  B_neg <- abs(sum(dif[dif < 0]))
  beta_abs <- sum(abs(dif)) / q_sum
  
  # Calculate performance criteria
  rpearson <- cor(sim, obs, method = "pearson")
  rspearman <- cor(sim, obs, method = "spearman")
  bias <- mean_s - mean_o
  MAE <- mean(abs(sim - obs))
  if ("BE" %in% crit) BE <- 1 - abs((sum_o - sum_s) / sum_o)
  if ("NSE" %in% crit || "C2M" %in% crit)
    NSE <- 1 - sum((sim - obs) ^ 2) / sum((obs - mean_o) ^ 2)
  if ("C2M" %in% crit) C2M <- NSE / (2 - NSE)
  if ("RMSE" %in% crit) RMSE <- sqrt(sum((sim - obs) ^ 2) / length(obs))
  if ("KGE" %in% crit)
    KGE <- 1 - sqrt(
      (sf[1] * (rpearson - 1)) ^ 2 + (sf[2] * (alpha - 1)) ^ 2 + (sf[3] * (beta - 1)) ^ 2
    )
  if ("KGE_abs" %in% crit)
    KGE_abs <- 1 - sqrt(
      (sf[1] * (rpearson - 1)) ^ 2 + (sf[2] * (alpha - 1)) ^ 2 + (sf[3] * beta_abs) ^ 2
    )
  if ("KGE_m" %in% crit)
    KGE_m <- 1 - sqrt(
      (sf[1] * (rpearson - 1)) ^ 2 + (sf[2] * (gamma - 1)) ^ 2 + (sf[3] * (beta - 1)) ^ 2
    )
  if ("KGE_m_abs" %in% crit)
    KGE_m_abs <- 1 - sqrt(
      (sf[1] * (rpearson - 1)) ^ 2 + (sf[2] * (gamma - 1)) ^ 2 + (sf[3] * beta_abs) ^ 2
    )
  if ("KGE_m2" %in% crit)
    KGE_m2 <- 1 - sqrt(
      (sf[1] * (rpearson - 1)) ^ 2 + (sf[2] * (alpha - 1)) ^ 2 + (sf[3] * beta_n) ^ 2
    )
  if ("KGENP" %in% crit)
    KGENP <-  1 - sqrt(
      (sf[1] * (rspearman - 1)) ^ 2 + (sf[2] * (alphaNP - 1)) ^ 2 + (sf[3] * (beta - 1)) ^ 2
    )
  if ("KGENP_abs" %in% crit)
    KGENP_abs <-  1 - sqrt(
      (sf[1] * (rspearman - 1)) ^ 2 + (sf[2] * (alphaNP - 1)) ^ 2 + (sf[3] * beta_abs) ^ 2
    )
  if ("LME" %in% crit)
    LME <- 1 - sqrt(((rpearson * alpha - 1) ^ 2 + (beta - 1) ^ 2))
  if ("LCE" %in% crit)
    LCE <- 1 - sqrt(((rpearson * alpha - 1) ^ 2 + (rpearson / alpha - 1) ^ 2 + (beta - 1) ^ 2))
  if ("CI" %in% crit)
    CI <- bias / MAE
  
  # Store desired criteria
  for (c in crit) {
    output[[c]] <- get(c)
  }
  
  # Return output, vector or list
  if (format == "vector") return(unlist(output))
  return(output)
  
}