
get_rank_class <- function(value, cls) {
  for (i in 1:length(cls)) {
    if (value < cls[[i]]) {
      return (i)
    }
  }
}
