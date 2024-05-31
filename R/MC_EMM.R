MC_EMM <- function(T, max_t = 1000) {
  t <- 0:max_t
  C <- (1 / T) * exp(-(t - 0.5) / T)
  MC <- C / sum(C)
  return(MC)
}
