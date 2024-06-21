MC_EMM <- function(T, n, max_t = 1000) {
  # "n" here is useless, it is a lazy workaround for the do.call call in model_tracer() function
  t <- 1:max_t
  C <- (1 / T) * exp(-(t - 0.5) / T)
  MC <- C / sum(C)
  return(MC)
}
