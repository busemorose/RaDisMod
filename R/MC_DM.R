MC_DM <- function(T, DP, max_t = 1000) {
  t <- 1:max_t

  a <- (t - 0.5) / T
  C <- (1 / (T * sqrt(4 * pi * DP * a))) * exp(-((1 - a) ^ 2) / (4 * DP * a))

  MC <- C / sum(C)
  return(MC)
}
