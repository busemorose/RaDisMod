NSE <- function(sim, obs, na.rm = TRUE, allow_NA = FALSE) {
  
  if (allow_NA) {
    na <- is.na(obs)
    
    obs <- obs[!na]
    sim <- sim[!na]
  }
  
  # Calculate mean sim and obs
  mean_o = mean(obs, na.rm = na.rm)
  
  NSE <- 1 - sum((sim - obs) ^ 2) / sum((obs - mean_o) ^ 2)
  
  return(NSE)
}
