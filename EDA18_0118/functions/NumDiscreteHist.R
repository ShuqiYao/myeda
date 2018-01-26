
NumDiscreteHist <- function (x, binwidth ,...){
  # num <- sapply(SplitColType$discrete,is.numeric)
  # numeric <- discrete[,which(num)]
  # 
  if (!is.numeric(x)) (print("This col is not numrical discrete variable!"));
  if (missing(binwidth)) {
    binwidth = 11
    print ("Please enter the binwidth you want, otherwise default binwidth equal to 1")
  };
  x = data.frame(x)
  plot <- ggplot(x, aes_string(x = names(x))) +
    geom_histogram(colour = "black", alpha = 0.4, show.legend = TRUE, binwidth = binwidth,...) 
  print(plot)
  }


# test 
# x = discrete$dwd_intopieces.income_type
# 
# 
# NumDiscreteHist(x, binwidth = 0.5)
