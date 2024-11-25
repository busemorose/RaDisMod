KGEABS <- function(sim, obs, na.rm = TRUE, allow_NA = FALSE) {
  
  if (allow_NA) {
    na <- is.na(obs)
    
    obs <- obs[!na]
    sim <- sim[!na]
  }
  
  # Calculate sd sim and obs
  sd_s = sd(sim, na.rm = na.rm)
  sd_o = sd(obs, na.rm = na.rm)
  
  # KGEABS components
  dif <- sim - obs
  q_sum <- sum(obs)
  B_pos <- abs(sum(dif[dif > 0]))
  B_neg <- abs(sum(dif[dif < 0]))
  beta_abs <- sum(abs(dif)) / q_sum
  
  alpha <-  sd_s / sd_o
  rpearson <- cor(sim, obs, method = "pearson")
  
  KGEABS <- 1 - sqrt((rpearson - 1) ^ 2 + (alpha - 1) ^ 2 + beta_abs ^ 2)
  
  return(KGEABS)
}
