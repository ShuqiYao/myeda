

DiscreteMethod <- function(data,save_path = getwd(),...) {
  
  date <- Sys.Date() #获取时间
  discrete <- SplitColType(data)$discrete
  
  sapply(seq_along(discrete),function(j,...){
    x <- discrete[,j]
    colname <- colnames(discrete)[j]
    
    # s_path <- paste(getwd(),'/','output',sep = '') #把输出总目录缩短写
    dir.create(paste(save_path,'/output','/',date,'/','col',sep = ''))
    save_path <- paste(save_path,'/output','/',date,'/','col',sep = '')
    
    file_name <- paste(colname,".html",sep = '') #文件名称
    if(file.exists(file_name)) {file.remove(file_name)}
    
    render_discrete(x,output_file = file_name,output_dir=save_path,colname=colname,...)
  })
}

#DiscreteMethod(intopieces)
