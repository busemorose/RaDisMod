MC_PEM <- function(T, n, max_t = 1000) {
  t <- 1:max_t
  C <- ifelse((t - 0.5) >= (T * log(n)), (n / T) * exp(-(t - 0.5) / T), 0)
  MC <- C / sum(C)
  return(MC)
}
