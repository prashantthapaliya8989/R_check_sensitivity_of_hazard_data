get_avg <- function(norm) {
  avg <- list()
  avg[[1]] <- norm[[1]]
  # print (avg[[1]])
  empty1 <- c()
  # print (typeof(empty1))
  for (i in 1:length(norm[[1]])) {
    sum <- 0
    for (index in 2:length(norm)) {
      value <- unlist(norm[[index]][i])
      sum <- sum + value
    }
    average <- sum / (length(norm)-1)
    empty1 <- append(empty1, average)
  }
  avg[[2]] <- empty1
  
  return (avg)
}
