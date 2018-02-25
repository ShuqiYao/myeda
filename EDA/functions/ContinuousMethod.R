

ContinuousMethod <- function(data, save_path = getwd(),graph_type ="histogram",binwidth,...) {
  
  date <- Sys.Date() #获取时间

  continuous <- SplitColType(data)$continuous
  
  sapply(seq_along(continuous),function(j,...){
    x <- continuous[,j]
    colname <- colnames(continuous)[j]
    
    # s_path <- paste(getwd(),'/','output',sep = '') #把输出总目录缩短写
    dir.create(paste(save_path,'/output','/',date,'/','col',sep = ''))
    save_path <- paste(save_path,'/output','/',date,'/','col',sep = '')
    
    file_name <- paste(colname,".html",sep = '') #文件名称
    
    
    if(file.exists(file_name)) {file.remove(file_name)}
    
    
    render_continuous (x,graph_type ="all",binwidth,colname = colname,
                       output_file = file_name,output_dir=save_path,...)
  })
  
}
# render_continuous(x)
#   
#ContinuousMethod(intopieces)
