HU_triangle <- function(Tm, alpha, max_t = 1000) {
  t <- 0:max_t
  HU <- ifelse(t > Tm, 
               (2 / (alpha * Tm)) * ((alpha * Tm - t) / (alpha * Tm - Tm)),
               (2 * t) / (alpha * Tm ^ 2))
  HU <- ifelse(HU < 0, 0, HU)
  return(HU)
}