ContinuousCorrelation <- function(data,...){
  continuous <- SplitColType(data)$continuous

  continuous <- tbl_df(continuous)
  
  continu_mis <- data.table("feature" = names(continuous), "num_missing" = sapply(continuous, function(x) {sum(is.na(x))}))
  
  #calculate missing percentage, drop some variable which have large missing values and will affect the calculation of 
  #coefficients
  continu_mis[, feature := factor(feature, levels = feature[order(-rank(num_missing))])]
  continu_mis[, pct_missing := num_missing / nrow(continuous)]
  continu_mis <- tbl_df(continu_mis)
  drop_var <- dplyr::filter(continu_mis, pct_missing < 0.9)
  #only remain missing percentage less than 90%
  continuous_fill <- dplyr::select(continuous, one_of(as.character(drop_var$feature)))
  # data may contain unixtimestamp, which is treated as continuous, drop those meaning less variables
  continuous_fill <- dplyr::select(continuous_fill, -matches("time"))
  correlation <- data.frame(cor(continuous_fill,use="complete.obs"))
 
  # ignore self correlation
  correlation [correlation == 1] <- NA   
  seperate1 <- apply(correlation ,2,function(x){
    max(x, na.rm=T)
  })
  order_num <- order(seperate1,decreasing = T)
  if (length(unique(seperate1))<10){
    p= length(unique(seperate1))
  } else {
    p = 10
  }
  col_names <- names(continuous_fill)[order_num[1:p]]
  sub_set <- continuous[col_names]
  
  # make sure name in graph is fitable
  re_name<- sapply(seq_along(sub_set),function(j){
    split_name= strsplit(names(sub_set)[j],"\\.")
    col_name = unlist(split_name)[2]
  })
  
  names(sub_set)<- re_name
  
  
  if (!is.data.table(sub_set)) {sub_set <- data.table(sub_set)}
  ## Stop if no continuous features
  ## Calculate correlation and melt into tidy data format
  plot_data <- reshape2::melt(cor(sub_set,use="complete.obs"))
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
