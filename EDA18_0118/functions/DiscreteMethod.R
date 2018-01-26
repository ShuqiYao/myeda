

DiscreteMethod <- function(data,save_path = getwd(),...) {
  
  date <- Sys.Date() #获取时间
  discrete <- SplitColType(data)$discrete
  
  sapply(seq_along(discrete),function(j,...){
    x <- discrete[,j]
    # x_name <- names(discrete)[j]
    
    split_name <-strsplit(colnames(discrete),"\\.")
    k <- split_name[j]
    
    tabname <- unlist(k)[1]
    colname <- unlist(k)[2]
    
    # s_path <- paste(getwd(),'/','output',sep = '') #把输出总目录缩短写
    dir.create(paste(save_path,'/output','/',date,'/',tabname,'/','col',sep = ''))
    save_path <- paste(save_path,'/output','/',date,'/',tabname,'/','col',sep = '')
    
    file_name <- paste(colname,".html",sep = '') #文件名称
    if(file.exists(file_name)) {file.remove(file_name)}
    
    render_discrete(x,output_file = file_name,output_dir=save_path,colname=colname,...)
  })
}

#DiscreteMethod(intopieces)
