setwd('/data/yaoshuqi/r_file/Myscript/my_test')
rm(list=ls())
library(data.table)
library(RJDBC)
source('/data/yaoshuqi/r_file/Myscript/my_test/source_list.R')

s <- paste(" select * from dm_kg.dwd_intopieces where etl_date = '2018-01-10' limit 10000")
intopieces <- dbGetQuery(hiveconnection,s)

TablestatMethod(intopieces,getwd())
