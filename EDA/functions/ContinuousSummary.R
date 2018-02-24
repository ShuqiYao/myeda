
## 描述性统计分析
## 输入一个字段，列的向量，生成这个向量的描述性分析
ContinuousSummary <- function(x){
  # is_data_vector <- as.vector(x)
  
  # if (!require(psych)){ 
  #   install.packages(psych) 
  # }  
  # library(psych)
  
  # summary(x)
  n <- length(x)                    #样本数据个数
  m <- mean(x,na.rm = TRUE)                      #均值
  me <- median(x,na.rm = TRUE)                   #中位数
  mo <- names(table(x))[which.max(table(x))]  #众数
  sd <- sd(x,na.rm = T)                       #标准差
  v <- var(x, na.rm = T)                       #方差
  r <- max(x) - min(x)              #极差
  cv <- 100 * sd/m                  #变异系数
  css <- sum(x - m)^2               #样本校正平方和
  uss <- sum(x^2)                   #样本未校正平方和
  R1 <- quantile(x,0.75,na.rm = TRUE) - quantile(x,0.25,na.rm = TRUE)     #四分位差
  sm <- sd/sqrt(n)                              #标准误
  g1 <- n/((n-1)*(n-2)*sd^3)*sum((x-m)^3)/sd^3  #偏度系数
  g2 <- ((n*(n+1))/((n-1)*(n-2)*(n-3))*sum((x-m)^4)/sd^4 -(3*(n-1)^2)/((n-2)*(n-3))) #峰度系数
  data.frame(N=n,Mean=m,Median=me,Mode=mo,
             Std_dev=sd,Variance=v,Range=r,
             CV=cv,CSS=css,USS=uss,
             R1=R1,SM=sm,Skewness=g1,Kurtosis=g2,
             row.names=1)
}
#test
#ContinuousSummary(y)
