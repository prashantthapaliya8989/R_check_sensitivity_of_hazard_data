get_normalized_value <- function(value_list, min_max) {
  norm <- list()
  norm[[1]] <- value_list[[1]]
  for (i in 2:(length(value_list))) {
    empty <- c()
    for (index in 1:length(value_list[[1]])) {
      diff <- min_max[[i]][2] - min_max[[i]][1]
      value <- (value_list[[i]][index] - min_max[[i]][1])/diff
      empty[[index]] <- value
    }
    norm[[i]] <- empty
  }
  return (norm)
}