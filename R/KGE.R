KGE <- function(sim, obs, na.rm = TRUE, allow_NA = FALSE) {
  
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
  
  # KGE components
  alpha <-  sd_s / sd_o
  beta <- mean_s / mean_o
  rpearson <- cor(sim, obs, method = "pearson")
  
  KGE <- 1 - sqrt((rpearson - 1) ^ 2 + (alpha - 1) ^ 2 + (beta - 1) ^ 2)
  
  return(KGE)
}
