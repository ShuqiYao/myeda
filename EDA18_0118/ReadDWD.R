setwd('/data/yaoshuqi/r_file/Myscript/my_test')
rm(list=ls())
library(data.table)
library(RJDBC)
source('/data/yaoshuqi/r_file/Myscript/my_test/source_list.R')
s <- paste(" select * from dm_kg.dwd_intopieces where etl_date = '2018-01-10' limit 10000")
intopieces <- dbGetQuery(hiveconnection,s)

# 
# attach(intopieces)
# detach(intopieces)
# hist(dwd_intopieces.loan_year)
# tt <- as.data.frame(table(dwd_intopieces.loan_year))
intopieces[intopieces == "NULL"] <- NA
intopieces[intopieces == "null"] <- NA
intopieces[intopieces == "NA"] <- NA
intopieces[intopieces == "na"] <- NA
intopieces[intopieces == ""] <- NA

SplitColType(intopieces)
# 152 个double
# 以下是在splitcoltype中的内部函数
# 首先,遇到的问题是,R不能自动分辨什么是integer和double, 其同意识别所有数字为
# num, 也就是double. 
# 我的目的是区分整数和分数,然后对整数识别为离散变量
# ## 首先,把全是缺失值的字段排除出去
# # 然后创建了一个没有 全部缺失 的数据框exc_mis
# # 然后,在这个数据框下先识别了所有是分数的字段 du
# # 之后生成一个数据库,其包含所有为分数的字段
# # 在数据框上,对每个字段应用我的自定函数 (x==round(x)),其用来检测每个数据是否为整数
# # 然后将结果中为NA的,全部替换为FALSE,即缺失值为假.同时生成一个数据框
# # 然后对该数据框应用函数,字段内数据均为TRUE,返回一个布尔值的向量
# # 
# #然后将非 double的字段也并入discrete中去cbind部分
# # Find indicies for double features

is_data_table <- is.data.frame(data)
## Detect input data class
data_class <- typeof(intopieces)
## Set intopieces to data.table
if (!is_data_table) {intopieces <- data.frame(intopieces)}



## Find indicies for double features
all_missing_ind <- sapply(intopieces, function(x) {sum(is.na(x)) == length(x)})
all_missing <- intopieces[,which(all_missing_ind)]
exc_mis <- intopieces[,which(!all_missing_ind)]
du <- sapply(exc_mis, is.double)
dou <- exc_mis[,which(du)]
double <- sapply(dou, function(x) { x %% 1 == 0})
double <- data.frame(double)
double[is.na(double)] <- FALSE
ind <- sapply(double,function(x) {all(x) == TRUE})

## 或者将16 17 18  改为 
##  ind <- sapply(dou, function(x) { all(x == round(x))})
##  ind[is.na[ind]] <- FALSE
## 并且删除19
# ind <- sapply(double, function(x) {x == round(x)})


## Count number of discrete, continuous and all-missing features
n_all_missing <- sum(all_missing_ind)
n_continuous <- sum(!ind)
n_discrete <- ncol(intopieces) - n_continuous - n_all_missing
## Create object for continuous features
continuous <- dou[, which(!ind)]

## 去除连续型变量里的 时间, ID 的变量
time_variables <- select (continuous, matches("time"))
continuous <- select(continuous, -matches("time"))
continuous <- select(continuous, -matches("id"))



## Create object for discrete features
disc <- dou[, which(ind)]
did <- exc_mis[ ,which (!du)]
discrete <- cbind(disc,did)
discrete <- tbl_df(discrete)
# 暂时先这么处理
# 把名字这些没有意义的字段先都剔除出去
discrete <- select(discrete, -matches("address"))
discrete <- select(discrete, -matches("spouse_name"))
discrete <- select(discrete, -matches("staff_name"))
discrete <- select(discrete, -matches("author_name"))
discrete <- select(discrete, -matches("client_name"))
discrete <- select(discrete, -matches("team_name"))
discrete <- select(discrete, -matches("sys_oper_time"))
discrete <- select(discrete, -matches("overdue_timestamp"))
discrete <- select(discrete, -matches("updata_submit_time"))
discrete <- select(discrete, -matches("pieces_no_id"))

 
n_cat <- sapply(discrete, function(x) {length(unique(x))})
ign_ind <- which(n_cat > 0.8*nrow(intopieces))
discrete<- discrete[,!(names(discrete) %in% names(ign_ind))]
# 
# time <- select(discrete, matches("time"))
# ID <- select (discrete, matches("id"))






list(
  "num_discrete" = n_discrete,
  "num_continuous" = n_continuous,
  "num_all_missing" = n_all_missing)







# 
# all_missing_ind <- sapply(intopieces, function(x) {sum(is.na(x)) == length(x)})
# 
# 
# 
# ## check again
# classtype <- sapply(intopieces, class)
# table(classtype)
# # 43 numeric
# # no difference
# 
# ## check again
# classtype2 <- sapply(intopieces, typeof)
# table(classtype2)
# ## check all variables if they are int or float
# 
# fod <- sapply(intopieces, is.double)
# foo <- intopieces[,which(fod)]
# 
# ## 之前的这些改变并没有解决问题
# ##      问题是：
# ##            识别数值的离散型变量
# ## 之前的这些，还是会识别int的数据为double，连续型。
# 
# classtype3 <- sapply(intopieces, is.integer)
# table(classtype3)
# 
# 
# classtype4 <- apply(foo,2, function(x) { x == round(x)})
# classtype4
# # is.logical(classtype4)
# classtype4 <- data.table(classtype4)
# classtype5 <- sapply(classtype4,function(x) {all(x) == 'TRUE'})
# classtype5
# table(classtype5)
# 
# is.wholenumber <-
#   function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
# 
# 
# 
# ind <- sapply(intopieces, is.double)
# sum(ind)
# is.double(dwd_intopieces.shidi_app_time)
# 
# 
# ifinteger <- function(x){x %% 1==0}
# 
# 

#-----------------------------
#boxplot
library(ggplot2)
library(data.table)
library(reshape2)
library(Hmisc)
source('SplitColType.r')
source('continuousSummary.R')
# View(intopieces)
n <- nrow(continuous)
p <- ncol(continuous)
continuous <- SplitColType(intopieces)$continuous
d<- apply(continuous,2,describe,na.delete)
k<- apply(continuous, 2, chisq.test)
dddd<- data.frame(continuous$dwd_intopieces.cont_id)
chisq.test(dddd,na.rm)


# library(mice)
# imp<- mice(continuous, seed=1234)
# fit <-with(imp,lm(continuous$dwd_intopieces.month_income))
dpmis <- sapply(continuous,function(x){sum(is.na(x)) >= 0.8*length(x)}) 
continuous = continuous[,which(!dpmis)]
d<- apply(continuous,2,describe)
d




x <- as.data.frame(continuous$dwd_intopieces.repay_money)
plot<- lapply(seq_along(continuous), function(j,...){
  x <- as.data.frame(na.omit(continuous[, j]))
  ggplot(x, aes_string(x = names(x))) +
    geom_histogram(bins = 10, colour = "black", alpha = 0.4, show.legend = TRUE) +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = comma) +
    # geom_density(alpha=0.6)+
    ylab("Frequency")+
    labs(title=paste(names(x),'distribution histogram'))
  }
)
print(plot)



boxplot(x,main=paste("Box Plot for ",names(x)),range=1.5
        )
# +
#   stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="black")
x <- na.omit(x)


#----------------

discrete <- SplitColType(intopieces)$discrete
y <- discrete$dwd_intopieces.center_name
k<- as.data.frame(y)
ggplot(k,aes_string(x = names(k)))+
  geom_bar(color = "black",na.rm = TRUE,alpha=0.4)


#---连续型变量的相关关系矩阵----
# continuous <- SplitColType(intopieces)$continuous
continuous <- tbl_df(continuous)

continu_mis <- data.table("feature" = names(continuous), "num_missing" = sapply(continuous, function(x) {sum(is.na(x))}))

#计算字段缺失程度, 因为大量的缺失会导致无法计算相关关系,所以剔除一些影响的变量
continu_mis[, feature := factor(feature, levels = feature[order(-rank(num_missing))])]
continu_mis[, pct_missing := num_missing / nrow(continuous)]
continu_mis <- tbl_df(continu_mis)
drop_var <- filter(continu_mis, pct_missing < 0.9)
#只提取符合条件的(缺失值小于90%)的字段
continuous_fill <- select(continuous, one_of(as.character(drop_var$feature)))
# 时间的字段被识别为连续型, 但是时间的字段并没有什么含义, 剔除时间戳的字段
# 返回数据中不包含时间字段的数据
continuous_fill <- select(continuous_fill, -matches("time"))
# 求相关关系
correlation <- data.frame(cor(continuous_fill,use="complete.obs"))

#对所有相关系数取绝对值, 数值表示程度,不表示正负关系
correlation[correlation >=0.95] <- 0
correlation_abs <- apply(correlation,2, max)

# 对列相关关系进行加总
# seperate1 <- apply(correlation_abs, 2, mean)
# 排序
order_num <- order(correlation_abs,decreasing = T)
# 提取相关关系比较重要的排序前十的字段
col_names <- names(continuous_fill)[order_num[1:10]]

# 更具上叙的字段,生成一个子数据集
sub_set <- continuous[col_names]
# 使用子数据集生成相关关系图
if (!is.data.table(sub_set)) {sub_set <- data.table(sub_set)}
## Stop if no continuous features
## Calculate correlation and melt into tidy data format
plot_data <- reshape2::melt(cor(sub_set,use="complete.obs"))
## Create ggplot object
plot <- ggplot(plot_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2("Correlation Meter", low = "#0571b0", high = "#ca0020", space = "Lab") +
  xlab("Features") + ylab("Features") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90))
if (ncol(sub_set) <= 20) {plot <- plot + geom_text(aes(label = round(value, 2)))}
## Print plot object
print(plot)


#----离散变量的相关关系矩阵----
# discrete <- SplitColType(intopieces)$discrete
## Get number of categories for each feature


#----对原数据更改, 把各个字符型或者其他型的类型变量, 转换为有序的类别变量
discrete_all_num <- apply(discrete, 2, function(x){
  as.numeric(as.factor(x))
})


discrete_all_num <- data.frame(discrete_all_num)
#计算缺失的字段, 把缺失很多的剔除掉
# 原因, 大量缺失字段会导致无法计算相关关系
# 剔除缺失大于70%的字段

discrete_all_num.missing <- data.table("feature" = names(discrete_all_num), 
                                       "num_missing" = sapply(discrete_all_num, 
                                                              function(x) {sum(is.na(x))}))
discrete_all_num.missing[, feature := factor(feature, levels = feature[order(-rank(num_missing))])]
discrete_all_num.missing[, pct_missing := num_missing / nrow(discrete_all_num)]
discrete_all_num.missing <- tbl_df(discrete_all_num.missing)
remaind_var <- filter(discrete_all_num.missing, pct_missing < 0.7)
stop_var <- filter(discrete_all_num.missing, pct_missing >= 0.7)

# 生成剔除后的数据集
discrete_all_num <- select(discrete_all_num, one_of(as.character(remaind_var$feature)))


## 问题是有些字段全部都一样, 都为1, 都为0, 这样就会导致这个字段无法计算平均方差
## 所以要提出这些导致分母为0 的部分

# discrete_all_num 表中, 每一个字段中的唯一长度是否为1, 即为一个唯一字段
# 返回布尔值
move.allsame.length <- sapply(discrete_all_num,function(x){
  length(unique(x)) == 1
})
# 提取这些唯一长度均为1的字段, 
# 再次对这个数据检验, 全部的数据nrow(x)中, 是不是全部观测值都等这个唯一的字段

move.allsame.length.data <- discrete_all_num[move.allsame.length]
move.allsame.all <- sapply(move.allsame.length.data, function(x){
  all(x == unique(x))
})
# 提取这些确定了, 全部观测值都唯一的 字段
move.allsame.var <- names(move.allsame.all[move.allsame.all ==T])
# 从数据中剔除这些, 生成一个用来制作相关关系的数据集
discrete_all_num <- discrete_all_num[,!(names(discrete_all_num) %in% move.allsame.var)]


##---生成相关关系
#   皮尔逊相关关系
correlation.discrete_all_num <- cor(discrete_all_num)
# 把等于1 的删除了, 因为每一列都会有一个1的自相关, 所以剔除来保证取最大值的时候不会有问题
correlation.discrete_all_num [correlation.discrete_all_num == 1] <- NA
# 对相关关系矩阵, 每一列 求最大值, 不计算缺失值. 得出每一列的最大的相关关系

correlation.discrete_all_num_max <- apply(correlation.discrete_all_num ,2,function(x){
  max(x, na.rm=T)
})
# 对每列的相关关系排序, 返回了列的位置, 
correlation.discrete_all_num_max.order <- order(correlation.discrete_all_num_max,decreasing = T)
# p <- length(as.data.frame(discrete_pivot))*0.1
# 取列的个数
if (length(correlation.discrete_all_num_max.order) >= 20){
  p=20
} else {
  p= length(correlation.discrete_all_num_max.order)
}
# 取出前 p 个字段的名字
col_names <- names(correlation.discrete_all_num_max)[correlation.discrete_all_num_max.order[1:p]]
discrete_all_num <- tbl_df(discrete_all_num)
# 从数据集中取出前P个数据的数据集
sub_set <- select(discrete_all_num, one_of(as.character(col_names)))

# 对数据转置, 画图部分
plot_data <- reshape2::melt(cor(sub_set))

## Create ggplot object
plot <- ggplot(plot_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2("Correlation Meter", low = "#0571b0", high = "#ca0020", space = "Lab") +
  xlab("Features") + ylab("Features") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90))
if (ncol(sub_set) <= 50) {plot <- plot + geom_text(aes(label = round(value, 2)))}
## Print plot object
print(plot)








# if (!is.data.table(data)) {data <- data.table(data)}
# ## Stop if no discrete features
# if (SplitColType(data)$num_discrete == 0) stop("No Discrete Features")
n_cat <- sapply(discrete, function(x) {length(unique(x))})
ign_ind <- which(n_cat > 50)
if (length(ign_ind) > 0) {
  set(discrete, j = ign_ind, value = NULL)
  message(length(ign_ind), " columns ignored with more than 50", " categories.\n", paste0(names(ign_ind), ": ", n_cat[ign_ind], " categories\n"))
}

## Calculate categorical correlation and melt into tidy data format
discrete <- as.data.table(discrete)



ddm <- apply(discrete, 2, function(x){
  as.numeric(as.factor(x))
})





# tes <- select(discrete, matches('st'))
#计算字段缺失程度, 因为大量的缺失会导致无法计算相关关系,所以剔除一些影响的变量
discr_mis <- data.table("feature" = names(discrete), "num_missing" = sapply(discrete, function(x) {sum(is.na(x))}))

discr_mis[, feature := factor(feature, levels = feature[order(-rank(num_missing))])]
discr_mis[, pct_missing := num_missing / nrow(discrete)]
discr_mis <- tbl_df(discr_mis)
drop_var <- filter(discr_mis, pct_missing < 0.8)
#只提取符合条件的(缺失值小于90%)的字段
discrete_fill <- select(discrete, one_of(as.character(drop_var$feature)))
# 时间的字段被识别为连续型, 但是时间的字段并没有什么含义, 剔除时间戳的字段
# 返回数据中不包含时间字段的数据
discrete_fill <- select(discrete_fill, -matches("date"))
discrete_fill <- select(discrete_fill, -matches("load_stamp"))
kl <- which(!(sapply(discrete_fill, is.character)))
discrete_fill <- select(discrete, one_of(as.character(names(kl))))

# correlation <- cor(discrete_fill, method='kendall', use = "complete.obs")






discrete_fill <- as.data.table(discrete_fill)
discrete_fill[, discrete_id := seq(nrow(discrete_fill))]
discrete_pivot <- Reduce(
  function(x, y) {merge(x, y, by = "discrete_id")},
  lapply(names(discrete_fill)[names(discrete_fill) != "discrete_id"], function(x) {
    dcast.data.table(discrete_fill, discrete_id ~ paste0(x, "_", get(x)), length, value.var = "discrete_id")
  })
)
discrete_pivot[, discrete_id := NULL]

correlation <- data.frame(cor(discrete_pivot,use="complete.obs", method='spearman'))
correlation_abs <- apply(correlation,2,abs)
seperate1 <- apply(correlation_abs, 2, mean)
order_num <- order(seperate1,decreasing = T)
p <- length(as.data.frame(discrete_pivot))*0.1
col_names <- names(discrete_pivot)[order_num[1:p]]
discrete_pivot <- data.frame(discrete_pivot)
sub_set <- discrete_pivot[col_names]


plot_data <- reshape2::melt(cor(discrete_pivot,use="complete.obs"))


## Create ggplot object
plot <- ggplot(plot_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2("Correlation Meter", low = "#0571b0", high = "#ca0020", space = "Lab") +
  xlab("Features") + ylab("Features") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90))
if (ncol(discrete_pivot) <= 20) {plot <- plot + geom_text(aes(label = round(value, 2)))}
## Print plot object
print(plot)


#----直方图----
# if (!is.numeric(x)) (stop("This col is not numrical discrete variable!"));
if (missing(binwidth)) print('Please set binwidth, otherwidth default binwidth is (max-min)/10')
library(scales)
x <- continuous$dwd_intopieces.product_area
is_data_table <- is.data.frame(x)
if (!is_data_table) {x <- data.frame(x)}
bin_def <- (max(na.omit(x))-min(na.omit(x)))/10
if (bin_def <= 0.5) bin_def <- 0.5

plot <- ggplot(x, aes_string(x = names(x))) +
  geom_histogram(binwidth = bin_def, colour = "steelblue", alpha = 0.4, show.legend = TRUE) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  # geom_density()+
  ylab("Frequency")+
  labs(title=" ")



# ----离散变量 信息熵----
discrete <- SplitColType(intopieces)$discrete

discrete_all_num <- apply(discrete, 2, function(x){
  as.numeric(as.factor(x))
})

discrete_all_num <- data.frame(discrete_all_num)

discrete.unique.cata <- sapply(discrete_all_num, function(x){
  length(unique(x))
})

discrete.unique.cata <- data.frame(discrete.unique.cata)

dd <- sapply(discrete_all_num, function (x){
  y <- na.omit(x)
  e <- entropy(y)
  return(e)
})

dd_df<- data.frame(dd)

ggplot(dd_df, aes(dd_df$dd))+
  geom_bar(stat='identity')
