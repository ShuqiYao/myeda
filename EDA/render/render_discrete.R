render_discrete <- function(x,output_file,output_dir,colname,...) {
  rmarkdown::render(
    input = "./rmd_template/report_discrete.Rmd",
    output_file = output_file,
    output_dir = output_dir,
    params = list(data = x, colname = colname)
  )
  
}

#rendermy_fuckingresult(y,graph_type = "all",binwidth = 100,output_file = "dddd.html")
