#! /path/Rscript


args<-commandArgs(T)
args<-c('./data/germancredit.csv','/Users/yaoshuqi/RProject/myeda/EDA')
source('source_list.R')
## Read csv data source here 
df <- fread(args[1],data.table=F)
## put input arguments here


SetNa(df)
TablestatMethod(df,save_path=args[2])
ContinuousMethod(df,save_path= args[2])
DiscreteMethod(df,save_path=args[2])


