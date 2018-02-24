render_continuous <- function(x, graph_type = "histogram",binwidth,output_file="report.html",output_dir=getwd(),...) {
  render(
    input = "/data/yaoshuqi/r_file/Myscript/my_test/rmd_template/report_continuous.Rmd",
    output_file = output_file,
    output_dir = output_dir,
    params = list(data = x, graph_type = "histogram")
  )
  
}
#test
#rendermy_fuckingresult(y,graph_type = "all",binwidth = 100,output_file = "dddd.html")
