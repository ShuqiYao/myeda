DetectDup <- function(data) {
  ## Check if input is data.table
  is_data_table <- is.data.table(data)
  ## Set data to table, if data is not table
  if (!is_data_table) {data <- data.table(data)}
  
  Duplicate <- sapply(data, function(x) {sum(duplicated(x))})
  return(
    as.data.frame(Duplicate)
  )
}
