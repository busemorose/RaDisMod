RMSE <- function(sim, obs, na.rm = TRUE, allow_NA = FALSE) {
  
  if (allow_NA) {
    na <- is.na(obs)
    
    obs <- obs[!na]
    sim <- sim[!na]
  }
  
  RMSE <- sqrt(sum((sim - obs) ^ 2) / length(obs))
  
  return(RMSE)
}
