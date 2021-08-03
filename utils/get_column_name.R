get_column_name <- function(shp) {
  
  df <- data.frame(readOGR(shp))
  print ("The columns of the table are: ")
  count = 1
  column_List = c()
  for (i in 1:length(colnames(df))) {
    print(paste(as.integer(count), '  ' , colnames(df[i])))
    count <- count + 1
    column_List <- c(column_List, colnames(df[i]))
  }
  return (column_List)
}