get_min_max <- function(value_list) {
  min_max <- list()
  for(i in 1:length(value_list)){
    empty <- c()
    minimum <- min(value_list[[i]])
    # print (minimum)
    maximum <- max(value_list[[i]])
    # print (maximum)
    empty <- c(empty, minimum, maximum)
    min_max[[i]] <- empty
  }
  return (min_max)
}
