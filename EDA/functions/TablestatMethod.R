

TablestatMethod <- function(data, save_path) {
  
  date <- Sys.Date() 
  if(!is.data.frame(data)) {data<-data.frame(data)}
  
  dir.create(paste(save_path,'/output','/',date,'/',sep = ''))
  save_path <- paste(save_path,'/output','/',date,'/',sep = '')
  
  file_name <- paste("result.html",sep = '') 
  
  
  if(file.exists(file_name)) {file.remove(file_name)}
  render_tablestat (data,output_file = file_name,output_dir=save_path)
}

