render_continuous <- function(x, graph_type = "all", 
                              binwidth,output_file="report.html",colname,
                              output_dir=getwd(),...) {
  render(
    input = "./rmd_template/report_continuous.Rmd",
    output_file = output_file,
    output_dir = output_dir,
    params = list(data = x, graph_type = graph_type,colname = colname)
  )
  
}
#test
#rendermy_fuckingresult(y,graph_type = "all",binwidth = 100,output_file = "dddd.html")
