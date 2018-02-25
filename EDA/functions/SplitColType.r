SplitColType <- function(data) {
  is_data_table <- is.data.frame(data)
  data_class <- typeof(data)
  if (!is_data_table) {data <- data.frame(data)}
  ## Find indicies for double features
  all_missing_ind <- sapply(data, function(x) {sum(is.na(x)) == length(x)})
  all_missing <- data[,which(all_missing_ind)]
  exc_mis <- data[,which(!all_missing_ind)]
  ind <- sapply(exc_mis, is.numeric)

  ## Count number of discrete, continuous and all-missing features
  n_all_missing <- sum(all_missing_ind)
  n_continuous <- sum(ind)
  n_discrete <- ncol(data) - n_continuous - n_all_missing
  ## Create object for continuous features
  continuous <- data[, which(ind)]
  discrete <- exc_mis[ ,which (!ind)]
  
  return(
    list(
      "all_missing" = all_missing,
      "discrete" = discrete,
      "continuous" = continuous,
      "num_discrete" = n_discrete,
      "num_continuous" = n_continuous,
      "num_all_missing" = n_all_missing
    )
  )
}

