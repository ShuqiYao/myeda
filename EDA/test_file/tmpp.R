

ContinuousMethod <- function(x, graph_type ="histogram",...) {

  # descriptive summary
  summary<- ContinuousSummary(x)
  
  # plotall <- function(x,...) {
  #   plot = list(ContinuousHisto (x,...), ContinuousBox(x,...))
  #   print(plot)
  # graph analysis
  if (graph_type == "histogram") {
    plot <- ContinuousHisto(x,...)
    
  } else if (graph_type == "boxplot") {
    plot <- ContinuousBox(x,...)
    
  } else if (graph_type == "all") {
     plot1<- ContinuousBox(x,...)
     plot2<- ContinuousHisto(x,...)
   }
  
  # switch (graph_type,
  #   histogram = ContinuousHisto(x,...),
  #   boxplot = ContinuousBox(x,...),
  #   cat(graph_type,"is not a recognized type\n")
  # )
  #methods of test
  test <- ContinuousTests(x)
  
  result <- list(summary,test)
  return(result)
  return(plot)
  return(plot1,plot2)
  # return(
  #   list(
  #     summary=summary,
  #     test=test
  #     
  #   )
  # )
}

ContinuousMethod(y,graph_type = 'all', binwidth = 1000)



output_file ='ss.html'
output_dir= getwd()
render(
      input = "/data/yaoshuqi/r_file/Myscript/my_test/rmd_template/report_continuous.Rmd",
      output_file = output_file,
      output_dir = output_dir,
      params = list(data = x,graph_type = "histogram")
    )