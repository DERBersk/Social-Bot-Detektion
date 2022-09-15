#Funktion zur Erstellung eines dataframes, welcher den Durchschnitt der Ober, der Unter, den gesamtdurchschnitt und die Extrema jedes Jahres beinhaltet
createLowerUpperEachYear <- function(column, data) {
  print("createLowerUpperEachYear called")
  if (length(data[, paste0(column, 'O')]) == 0) {
    return("ERROR")
  } else{
    #extract list of all years
    years = names(table(year(data$Publikationsjahr)))
    #run through list
    result = data.frame(
      meanU = c(),
      meanO = c(),
      avg = c(),
      low = c(),
      high = c()
    )
    for (year in years) {
      #extract every entry in with year
      yearData <- subset(data, year(data$Publikationsjahr) == year)
      result = rbind(result,
                     data.frame(
                       meanU = c(mean(yearData[, paste0(column, 'U')], na.rm = TRUE)),
                       meanO = c(mean(yearData[, paste0(column, 'O')], na.rm = TRUE)),
                       avg = c(median((
                         yearData[, paste0(column, 'U')] + yearData[, paste0(column, 'O')]
                       ) / 2, na.rm = TRUE)),
                       low = c(min(yearData[, paste0(column, 'U')], na.rm = TRUE)),
                       high = c(max(yearData[, paste0(column, 'O')], na.rm = TRUE)),
                       year = year
                     ))
    }
    return(result)
  }
}

# Funktion zur Visualisierung des Dataframes aus createLowerUpperEachYear
createLowerUpperBP <- function(column, data) {
  lowerUpper <- createLowerUpperEachYear(column, data)
  ggplot(
    lowerUpper,
    aes(
      x = year,
      ymin = low,
      lower = meanU,
      middle = avg,
      upper = meanO,
      ymax = high,
      fill = year
    )
  ) +
    geom_boxplot(stat = "identity",
                 alpha = 0.3,) +
    theme(legend.position = "none",text=element_text(family="CMU Serif")) +
    scale_fill_brewer(palette = "Paired") +
    labs(
      x = "Jahr",
      y = paste0(str_to_title(column, locale = "de"), " Wert"),
      title = paste0("Analyse von ", str_to_title(column, locale = "de"), " über Zeit")
    )
}
