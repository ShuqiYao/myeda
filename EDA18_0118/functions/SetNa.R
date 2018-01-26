SetNa <- function(data){
  data[data == "NULL"] <- NA
  data[data == "Null"] <- NA
  data[data == "null"] <- NA
  data[data == "Na"]   <- NA
  data[data == "na"]   <- NA
  data[data == ""]     <- NA
}