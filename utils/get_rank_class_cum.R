get_rank_class_cum <- function(value, cls) {
  is_selected <- FALSE
  to_return <- c()
  for (i in 1:length(cls)){
    if (value < cls[[i]]){
      is_selected = TRUE
    }
    if (is_selected){
      to_return <- append(to_return, i)
    }
  }
  return (to_return)
}
