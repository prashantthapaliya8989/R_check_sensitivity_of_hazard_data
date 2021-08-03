get_column_value_ac_ex_sen <- function(shp, column_name){
  filter_index_list <- c()
  filter_list <- c()
  select_index <- as.integer(readline(prompt = "Enter the index of subbasin: "))
  filter_index_list <- c(filter_index_list, select_index)
  filter_list <- c(filter_list, column_name[select_index])
  select_index <- as.integer(readline(prompt = "Enter the index of AC/Exposure/Sensitivity: "))
  filter_index_list <- c(filter_index_list, select_index)
  filter_list <- c(filter_list, column_name[select_index])

  col_val <- list()
  for (i in 1:length(filter_list)) {
    df <- data.frame(readOGR(shp))
    indi_col_val <- df[,which(colnames(df) %in% filter_list[i])]
    if (is.character(indi_col_val)){
      int_indi_col_val <- as.integer(unlist(strsplit(indi_col_val,",")))
      col_val[[i]] <- int_indi_col_val
    }
    else {
      col_val[[i]] <- indi_col_val
    }
    #print (col_val)
  }
  names(col_val) <- filter_list
  return(col_val)
}