
DiscreteCorrelation <- function(data,...){

  
discrete <- SplitColType(data)$discrete
## Get number of categories for each feature

# if (!is.data.table(data)) {data <- data.table(data)}
# ## Stop if no discrete features
# if (SplitColType(data)$num_discrete == 0) stop("No Discrete Features")


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

# 改一下字段名称, 让图像里头小点
re_name<- sapply(seq_along(sub_set),function(j){
  split_name= strsplit(names(sub_set)[j],"\\.")
  col_name = unlist(split_name)[2]
})

names(sub_set)<- re_name


# 对数据转置, 画图部分

plot_data <- reshape2::melt(cor(sub_set,...))


## Create ggplot object
plot <- ggplot(plot_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2("Correlation Meter", low = "#0571b0", high = "#ca0020", space = "Lab") +
  xlab("Features") + ylab("Features") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90))
if (ncol(sub_set) <= 50) {plot <- plot + geom_text(aes(label = round(value, 2)))}
## Print plot object
print(plot)
}
