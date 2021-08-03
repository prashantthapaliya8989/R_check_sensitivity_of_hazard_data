get_sd_and_compute_climate <- function(value_list) {
  sdev <- list()
  for (i in 2:length(value_list)) {
    value <- sd(value_list[[i]])
    sdev[[i-1]] <- value
  }
  # print (sdev)
  compute_list <- list()
  compute_list[[1]] <- value_list[[1]]
  for (i in 2:length(value_list)) {
    empty <- c()
    for (index in 1:length(value_list[[1]])) {
      random_num <- as.double(runif(1,-1000,1000)/1000)
      value <- value_list[[i]][index] + random_num * sdev[[i-1]]
      empty <- append(empty, value)
    }
    compute_list[[i]] <- empty
  }
  
  return(compute_list)
}
