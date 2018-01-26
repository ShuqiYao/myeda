#! /path/Rscript


args<-commandArgs(T)
source('source_list.R')
## put input arguments here


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


