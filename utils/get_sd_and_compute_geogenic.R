get_sd_and_compute_geogenic <- function(value_list) {
  sdev <- list()
  for (i in 2:length(value_list)) {
    value <- sd(value_list[[i]])
    sdev[[i-1]] <- value
  }
  compute_list <- list()
  compute_list[[1]] <- value_list[[1]]
  for (i in 2:length(value_list)) {
    empty <- c()
    for (index in 1:length(value_list[[1]])) {
      if (value_list[[i]][index] > 0.02){
        random_num <- as.double(runif(1,-1000,1000)/1000)
        value <- value_list[[i]][index] + random_num * sdev[[i-1]]
        empty <- append(empty, value)
        # print(paste(value_list[[i]][index], value))
        # break
      }
      else {
        value <- 0
        empty <- append(empty, value)
      }

    }
    compute_list[[i]] <- empty
  }
  
  return(compute_list)
}

