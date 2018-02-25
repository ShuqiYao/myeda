
DiscreteCorrelation <- function(data,...){

  discrete <- SplitColType(data)$discrete
  #change data to ordinal data
  discrete_all_num <- apply(discrete, 2, function(x){
    as.numeric(as.factor(x))
  })
  
  
  discrete_all_num <- data.frame(discrete_all_num)
  #drop large missing values variable
  
  discrete_all_num.missing <- data.table("feature" = names(discrete_all_num), 
                                         "num_missing" = sapply(discrete_all_num, 
                                                                function(x) {sum(is.na(x))}))
  discrete_all_num.missing[, feature := factor(feature, levels = feature[order(-rank(num_missing))])]
  discrete_all_num.missing[, pct_missing := num_missing / nrow(discrete_all_num)]
  discrete_all_num.missing <- tbl_df(discrete_all_num.missing)
  remaind_var <- dplyr::filter(discrete_all_num.missing, pct_missing < 0.7)
  stop_var <- dplyr::filter(discrete_all_num.missing, pct_missing >= 0.7)
  
  # dataset for calculation
  discrete_all_num <- dplyr::select(discrete_all_num, one_of(as.character(remaind_var$feature)))
  
  # test if variable only contain 1 unique value and drop them
  move.allsame.length <- sapply(discrete_all_num,function(x){
    length(unique(x)) == 1
  })
  move.allsame.length.data <- discrete_all_num[move.allsame.length]
  move.allsame.all <- sapply(move.allsame.length.data, function(x){
    all(x == unique(x))
  })
  move.allsame.var <- names(move.allsame.all[move.allsame.all ==T])
  discrete_all_num <- discrete_all_num[,!(names(discrete_all_num) %in% move.allsame.var)]
  
  
  # Pearson
  correlation.discrete_all_num <- cor(discrete_all_num)
  correlation.discrete_all_num [correlation.discrete_all_num == 1] <- NA
  correlation.discrete_all_num_max <- apply(correlation.discrete_all_num ,2,function(x){
    max(x, na.rm=T)
  })
  correlation.discrete_all_num_max.order <- order(correlation.discrete_all_num_max,decreasing = T)
  if (length(correlation.discrete_all_num_max.order) >= 20){
    p=20
  } else {
    p= length(correlation.discrete_all_num_max.order)
  }
  col_names <- names(correlation.discrete_all_num_max)[correlation.discrete_all_num_max.order[1:p]]
  discrete_all_num <- tbl_df(discrete_all_num)
  sub_set <- dplyr::select(discrete_all_num, one_of(as.character(col_names)))

  re_name<- sapply(seq_along(sub_set),function(j){
    split_name= strsplit(names(sub_set)[j],"\\.")
    col_name = unlist(split_name)[2]
  })
  
  names(sub_set)<- re_name
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
