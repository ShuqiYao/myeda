

TablestatMethod <- function(data, save_path) {
  
  date <- Sys.Date() #获取时间
  if(!is.data.frame(data)) {data<-data.frame(data)}
  k <- strsplit(colnames(data),"\\.")[1]
  tabname <- unlist(k)[1]
  dir.create(paste(save_path,'/output','/',date,'/',tabname,sep = ''))
  save_path <- paste(save_path,'/output','/',date,'/',tabname,sep = '')
  
  file_name <- paste(tabname,".html",sep = '') #文件名称
  
  
  if(file.exists(file_name)) {file.remove(file_name)}
  render_tablestat (data,output_file = file_name,output_dir=save_path)
}

# TablestatMethod(intopieces,getwd(mial))
