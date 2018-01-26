

DiscreteSummary <- function(x) {
  
  # is_data_vector <- as.vector(x)
  # 
  # if (!require(Hmisc)){ 
  #   install.packages(Hmisc) 
  # }
  # 
  # library(Hmisc)
   x<-factor(x)
   describe(x)
  # a <- table(x)
  # a<- as.vector(a)
  # b <- prop.table(table(x))*100
  # b <- as.vector(b)
  # t <- data.frame(a,b)
  # names(t)<-c("Frequency","Proportion")
  # return(t)
}

## test
# DiscreteSummary(x)
