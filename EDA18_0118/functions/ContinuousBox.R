## box-plot
ContinuousBox <- function(x,...) {
  # plot1 <- boxplot(x,main=paste("Box Plot for ",names(x)),...)
  # print(plot1)
  # return(invisible(boxplot.stats(plot1)))
  boxplot(x,main=paste("Box Plot for ",names(x)),...)
}

# #test
# ContinuousBox(y)
