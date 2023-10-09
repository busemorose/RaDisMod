HU_gamma <- function(shape, rate, max_t = 1000) {
  t <- 0:max_t
  U <- dgamma(t, shape = shape, rate = rate)
  HU <- U / sum(U)
  return(HU)
}