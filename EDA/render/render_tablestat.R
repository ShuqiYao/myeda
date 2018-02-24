render_tablestat <- function(data,output_file,output_dir) {
  rmarkdown::render(
    input = "./rmd_template/render_tablestat.Rmd",
    output_file = output_file,
    output_dir = output_dir,
    params = list(data = data)
  )
  
}
