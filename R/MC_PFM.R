MC_PFM <- function(T, Cin) {
  T_floor <- floor(T)
  sim <- data.table::shift(Cin, T_floor, fill = 0)
  return(sim)
}
