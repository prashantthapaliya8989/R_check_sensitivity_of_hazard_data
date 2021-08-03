array_rank_transform <- function(arr){
  sorted_list = sort(arr) #[1,3,3,3,4,9]
  rank = 1
  sorted_rank_list = list(1)
  for (i in 1:(length(sorted_list)-1)){
    if (sorted_list[[i]] != sorted_list[[i+1]]) {
      rank = rank + 1
    }
    sorted_rank_list <- append(sorted_rank_list, rank)
  }
  rank_list <- list()

  for (item in arr){
    for (i in 1:length(sorted_list)) {
    #for (element in (sorted_list)) {
      if (sorted_list[[i]] == item) {
        value <- i #sorted_rank_list[[i]]
        rank_list <- append(rank_list, value)
        break
      }
    }
  }
  return (rank_list)
}
