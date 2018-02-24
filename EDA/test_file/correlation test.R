ContinousCorrelation <- function(data){
  continuous <- SplitColType(data)$continuous
  correlation <- data.frame(cor(continuous))
  
  seperate1 <- apply(continuous, 2, mean)
  
  order_num <- order(seperate1,decreasing = T)
  
  col_names <- names(continuous)[order_num[1:10]]
  
  sub_set <- continuous[col_names]
  
  if (!is.data.table(sub_set)) {sub_set <- data.table(sub_set)}
  ## Stop if no continuous features
  ## Calculate correlation and melt into tidy data format
  plot_data <- reshape2::melt(cor(sub_set, ...))
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