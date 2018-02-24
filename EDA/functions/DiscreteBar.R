## 条形图

DiscreteBar <- function(x,...) {
is_data_table <- is.data.frame(x)
if (!is_data_table) {x <- data.frame(x)}
if (sum(table(na.omit(x)))>500){
ggplot(x, aes_string(x = names(x))) +
  geom_bar(color = "black",na.rm = TRUE,alpha=0.4,...)+
  labs(title= paste(names(x),'barplot'))} else {
    print("There are too many categlories to plot barplot")
  }

}

#test

# DiscreteBar(x)
