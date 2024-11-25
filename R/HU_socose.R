HU_socose <- function(D, alpha, max_t = 1000) {
  t <- 0:max_t
  U <- (1 / (1.53 * D)) * (((t / D) ^ alpha) / (1 + (t / D) ^ (alpha * 2)))
  HU <- U / sum(U)
  return(HU)
}