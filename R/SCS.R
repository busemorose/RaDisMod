SCS <- function(P_cum, J) {
  ifelse(P_cum < (0.2 * J), 0, ((P_cum - 0.2 * J) ^ 2) / (P_cum + 0.8 * J))
}