add_unique_suffix <- function(types, n = 1) {
  table_types <- table(types)  # Count occurrences of each type

  result <- types  # Initialize result with the original types

  for (type in names(table_types)) {
    result[types == type] <- paste0(type, "_", seq_along(result[types == type]) + n)
  }

  return(result)
}
