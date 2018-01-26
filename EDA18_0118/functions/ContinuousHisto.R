##直方图
ContinuousHisto<- function(x,binwidth,...){
  # if (!is.numeric(x)) (stop("This col is not numrical discrete variable!"));
  if (missing(binwidth)) print('Please set binwidth, otherwidth default binwidth is (max-min)/10')
  library(scales)

  is_data_table <- is.data.frame(x)
  if (!is_data_table) {x <- data.frame(x)}
  bin_def <- (max(na.omit(x))-min(na.omit(x)))/10
  if (bin_def <= 0.5) bin_def <- 0.5
  
  plot <- ggplot(x, aes_string(x = names(x))) +
    geom_histogram(binwidth = bin_def, colour = "steelblue", alpha = 0.4, show.legend = TRUE,...) +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = comma) +
    # geom_density()+
    ylab("Frequency")+
    labs(title=" ")
    print(plot)
}


# # test
#ContinuousHisto(x)
