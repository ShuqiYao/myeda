#! /path/Rscript


args<-commandArgs(T)
source('source_list.R')
# args <- c("dwd_intopieces_repair_cs","2018-01-15",".")
s <- paste(" select * from ", args[1]," where etl_date = '",args[2],"' limit 10000",sep = "")
data <- dbGetQuery(hiveconnection,s)

# SetNa(data)
data[data == "NULL"] <- NA
data[data == "Null"] <- NA
data[data == "null"] <- NA
data[data == "Na"]   <- NA
data[data == "na"]   <- NA
data[data == ""]     <- NA

TablestatMethod(data,save_path=args[3])
ContinuousMethod(data,save_path= args[3])
DiscreteMethod(data,save_path=args[3])


