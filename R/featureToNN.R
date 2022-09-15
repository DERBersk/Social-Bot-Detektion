# Funktion zu der Erstellung eines dataframes mit dem relativen Auftreten einer Dimension in Abhängigkeit des gewählten technischen Ansatzes
createFeatureToNNAnsatz <- function(column, data) {
  if (length(data[, paste0(column, 'O')]) == 0) {
    return("ERROR")
  }
  result = data.frame(
    interval = c(
      "0 - 0.1",
      "0.1 - 0.2",
      "0.2 - 0.3",
      "0.3 - 0.4",
      "0.4 - 0.5",
      "0.5 - 0.6",
      "0.6 - 0.7",
      "0.7 - 0.8",
      "0.8 - 0.9",
      "0.9 - 1"
    )
  )
  for (i in 0:3) {
    curdata = subset(data, data[, "nnAnsatz"] == i)
    res = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    for (j in 1:length(curdata[, paste0(column, 'O')])) {
      if (is.na(curdata[, paste0(column, 'U')][j])) {
        next
      }
      vW = curdata[, paste0(column, 'U')][j] * 10 + 1
      while (vW < curdata[, paste0(column, 'O')][j] * 10 + 1) {
        res[vW] = res[vW] + 1
        vW = vW + 1
      }
    }
    result[, as.character(i)] = res / length(curdata[, "nnAnsatz"])
  }
  return(result)
}

# Funktion zur Visualisierung des Dataframes aus createFeatureToNNAnsatz
createFeatureToNNAnsatzMatrix <- function(column, data) {
  library(reshape2)
  library(ggplot2)
  df <- data.frame(createFeatureToNNAnsatz(column, data))
  colnames(df)[2:ncol(df)] <-
    c("Unüberwacht", "Semiüberwacht", "Überwacht", "Andere")
  gg <- melt(df, id = "interval")
  return(
    ggplot(gg, aes(
      y = variable, x = interval, fill = value
    )) +
      geom_tile(show.legend = FALSE) +
      geom_text(aes(label = round(value, 1),family="CMU Serif"), alpha = 0.6) +
      scale_fill_gradient(low = "#FFFF88", high = "#FF0000") +
      labs(y = "Ansätze", x = str_to_title(column,locale = "de")) +
      theme(legend.key = element_blank(),text=element_text(family="CMU Serif")) + coord_flip()
  )
}
