#' Create summary of continuous features
#' 
#' This function create descriptive summary of continuous features in the data
#' @param data imput data to get summary, in either \link{data.frame} or \link{data.table} format
#' @importFrom Hmisc describe
#' @import data.table
#' @export
#' @examples
#' #load packages
#' continuousSummary(life)

continuousSummary<- function(data) {
  # if (!is.data.frame(data)) {data <- data.frame(data)}
  # ## Stop if no continuous features
  # if (SplitColType(data)$num_continuous == 0) stop("No Continuous Features")
  ## Get continuous features
  # continuous <- SplitColType(data)$continuous
  # get summary of continuous variable
  # continuous<-data.table(continuous)
  result=describe(data,descript = 'Descriptive summary of Continuous Variables')
  result=t(result)
  result
}