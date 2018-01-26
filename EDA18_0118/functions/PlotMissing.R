PlotMissing <- function(data) {
  ## Declare variable first to pass R CMD check
  feature <- num_missing <- pct_missing <- group <- NULL
  ## Check if input is data.table
  is_data_table <- is.data.table(data)
  ## Detect input data class
  data_class <- class(data)
  ## Set data to data.table
  if (!is_data_table) {data <- data.table(data)}
  ## Extract missing value distribution
  missing_value <- data.table("feature" = names(data), "num_missing" = sapply(data, function(x) {sum(is.na(x))}))
  missing_value[, feature := factor(feature, levels = feature[order(-rank(num_missing))])]
  missing_value[, pct_missing := num_missing / nrow(data)]
  missing_value[pct_missing < 0.05, group := "Good"]
  missing_value[pct_missing >= 0.05 & pct_missing < 0.4, group := "OK"]
  missing_value[pct_missing >= 0.4 & pct_missing < 0.8, group := "Bad"]
  missing_value[pct_missing >= 0.8, group := "Remove"][]
  plot_variable <- subset(missing_value, group =="Bad"| group =="Remove")
  ## Set data class back to original
  if (!is_data_table) {class(missing_value) <- data_class}
  ## Create ggplot object
  if (nrow(plot_variable)>0){
  output <- ggplot(plot_variable, aes_string(x = "feature", y = "num_missing", fill = "group")) +
    geom_bar(stat = "identity", colour = "black", alpha = 0.4) +
    geom_text(aes(label = paste0(round(100 * pct_missing, 0), "%")), hjust = -0.15, size = 3.5) +
    scale_fill_manual("Group", values = c("Good" = "#1a9641", "OK" = "#a6d96a", "Bad" = "#fdae61", "Remove" = "#d7191c"), breaks = c("Good", "OK", "Bad", "Remove")) +
    scale_y_continuous(labels = comma) +
    theme(legend.position = c("bottom")) + coord_flip() +
    xlab("Features") + ylab("Number of missing rows")
  } else {
    print("Great! There is no variable missing over 40% of total number of observations")
  }
  ## Print plot
  print(output)
  ## Set return object
  return(invisible(missing_value))
}
