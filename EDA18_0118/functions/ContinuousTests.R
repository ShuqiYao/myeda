## several tests maybe can use here
ContinuousTests<- function(x){
## 卡方检验
x <- na.omit(x)

if (min(na.omit(x))<=0 & max(na.omit(x)<=0)) {
    chi_sum <- print('At least one variable is not 0 and positive')
  } else {
    chi_sum <- chisq.test(x)
  }
if (length(table(x))>1) {
  t_sum <- t.test(x)
  } else {
    t_sum <- print("not enough observation to do a t-test")
  }


return(list(
    chi_sum,
    t_sum))
## t检验

}



# #test
# ContinuousTests(y)
