KGENP <- function(sim, obs, na.rm = TRUE, allow_NA = FALSE) {
  
  if (allow_NA) {
    na <- is.na(obs)
    
    obs <- obs[!na]
    sim <- sim[!na]
  }
  
  # Calculate mean sim and obs
  mean_s = mean(sim, na.rm = na.rm)
  mean_o = mean(obs, na.rm = na.rm)
  
  # Calculate normalised flow duration curves
  fdc_s = sort(sim / (mean_s * length(sim)))
  fdc_o = sort(obs / (mean_o * length(obs)))
  
  # KGE components
  alphaNP <- 1 - 0.5 * sum(abs(fdc_s - fdc_o))
  beta <- mean_s / mean_o
  rspearman <- cor(sim, obs, method = "spearman")
  
  KGENP <-  1 - sqrt((rspearman - 1) ^ 2 + (alphaNP - 1) ^ 2 + (beta - 1) ^ 2)
  
  return(KGENP)
}
