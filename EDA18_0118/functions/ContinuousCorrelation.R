ContinuousCorrelation <- function(data,...){
  continuous <- SplitColType(data)$continuous

  continuous <- tbl_df(continuous)
  
  continu_mis <- data.table("feature" = names(continuous), "num_missing" = sapply(continuous, function(x) {sum(is.na(x))}))
  
  #计算字段缺失程度, 因为大量的缺失会导致无法计算相关关系,所以剔除一些影响的变量
  continu_mis[, feature := factor(feature, levels = feature[order(-rank(num_missing))])]
  continu_mis[, pct_missing := num_missing / nrow(continuous)]
  continu_mis <- tbl_df(continu_mis)
  drop_var <- filter(continu_mis, pct_missing < 0.9)
  #只提取符合条件的(缺失值小于90%)的字段
  continuous_fill <- select(continuous, one_of(as.character(drop_var$feature)))
  # 时间的字段被识别为连续型, 但是时间的字段并没有什么含义, 剔除时间戳的字段
  # 返回数据中不包含时间字段的数据
  continuous_fill <- select(continuous_fill, -matches("time"))
  # 求相关关系
  correlation <- data.frame(cor(continuous_fill,use="complete.obs"))
  
  #对所有相关系数取绝对值, 数值表示程度,不表示正负关系
  correlation_abs <- apply(correlation,2, abs)
  # 对列相关关系进行加总
  seperate1 <- apply(correlation_abs, 2, mean)
  # 排序
  order_num <- order(seperate1,decreasing = T)
  # 提取相关关系比较重要的排序前十的字段
  if (length(unique(seperate1))<10){
    p= length(unique(seperate1))
  } else {
    p = 10
  }
  col_names <- names(continuous_fill)[order_num[1:p]]
  # 更具上叙的字段,生成一个子数据集
  sub_set <- continuous[col_names]
  
  # 改一下字段名称, 让图像里头小点
  re_name<- sapply(seq_along(sub_set),function(j){
    split_name= strsplit(names(sub_set)[j],"\\.")
    col_name = unlist(split_name)[2]
  })
  
  names(sub_set)<- re_name
  
  
  if (!is.data.table(sub_set)) {sub_set <- data.table(sub_set)}
  ## Stop if no continuous features
  ## Calculate correlation and melt into tidy data format
  plot_data <- reshape2::melt(cor(sub_set,use="complete.obs", ...))
  ## Create ggplot object
  plot <- ggplot(plot_data, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2("Correlation Meter", low = "#0571b0", high = "#ca0020", space = "Lab") +
    xlab("Features") + ylab("Features") +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 90))
  if (ncol(sub_set) <= 20) {plot <- plot + geom_text(aes(label = round(value, 2)))}
  ## Print plot object
  print(plot)
  
}
