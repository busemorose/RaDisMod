MC_EPM <- function(T, n, max_t = 1000) {
  t <- 1:max_t
  C <- ifelse(t >= (1 - 1 / n), (n / T) * exp(((-n * (t - 0.5)) / T + n - 1)), 0)
  MC <- C / sum(C)
  return(MC)
}
