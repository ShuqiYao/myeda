SplitColType <- function(data) {
  ## Check if input is data.table
  is_data_table <- is.data.frame(data)
  ## Detect input data class
  data_class <- typeof(data)
  ## Set data to data.table
  if (!is_data_table) {data <- data.frame(data)}
  
  
  
  ## Find indicies for double features
  all_missing_ind <- sapply(data, function(x) {sum(is.na(x)) == length(x)})
  all_missing <- data[,which(all_missing_ind)]
  exc_mis <- data[,which(!all_missing_ind)]
  du <- sapply(exc_mis, is.double)
  dou <- exc_mis[,which(du)]
  double <- sapply(dou, function(x) { x %% 1 == 0})
  double <- data.frame(double)
  double[is.na(double)] <- FALSE
  ind <- sapply(double,function(x) {all(x) == TRUE})

  ## 或者将16 17 18  改为 
  ##  ind <- sapply(dou, function(x) { all(x == round(x))})
  ##  ind[is.na[ind]] <- FALSE
  ## 并且删除19
  # ind <- sapply(double, function(x) {x == round(x)})
  
  
  ## Count number of discrete, continuous and all-missing features
  n_all_missing <- sum(all_missing_ind)
  n_continuous <- sum(!ind)
  n_discrete <- ncol(data) - n_continuous - n_all_missing
  ## Create object for continuous features
  continuous <- dou[, which(!ind)]
  
  # ## 去除连续型变量里的 时间, ID 的变量 2018年1月16日
  # continuous <- select(continuous, -matches("time"))
  # continuous <- select(continuous, -matches("id"))
  # time_variables <- select (continuous, -matches("time"))
  
  ## Create object for discrete features
  disc <- dou[, which(ind)]
  did <- exc_mis[ ,which (!du)]
  discrete <- cbind(disc,did)
  
  # 暂时先这么处理
  # 把名字这些没有意义的字段先都剔除出去
  # discrete <- select(discrete, -matches("address"))
  # discrete <- select(discrete, -matches("spouse_name"))
  # discrete <- select(discrete, -matches("staff_name"))
  # discrete <- select(discrete, -matches("author_name"))
  # discrete <- select(discrete, -matches("client_name"))
  # discrete <- select(discrete, -matches("team_name"))
  # discrete <- select(discrete, -matches("sys_oper_time"))
  # discrete <- select(discrete, -matches("overdue_timestamp"))
  # discrete <- select(discrete, -matches("updata_submit_time"))
  # discrete <- select(discrete, -matches("pieces_no_id"))
  
  
  n_cat <- sapply(discrete, function(x) {length(unique(x))})
  ign_ind <- which(n_cat > 0.8*nrow(data))
  discrete<- discrete[,!(names(discrete) %in% names(ign_ind))]
  
  list(
    "num_discrete" = n_discrete,
    "num_continuous" = n_continuous,
    "num_all_missing" = n_all_missing)
  ## Set data class back to original
  # if (!is_data_table) {class(discrete) <- class(continuous) <- data_class}
  ## Set return object
  
  return(
    list(
      "all_missing" = all_missing,
      "discrete" = discrete,
      "continuous" = continuous,
      "num_discrete" = n_discrete,
      "num_continuous" = n_continuous,
      "num_all_missing" = n_all_missing
    )
  )
}
