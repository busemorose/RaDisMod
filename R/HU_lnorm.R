HU_lnorm <- function(meanlog, sdlog, max_t = 1000) {
  t <- 0:max_t
  U <- dlnorm(t, meanlog = meanlog, sdlog = sdlog)
  HU <- U / sum(U)
  return(HU)
}