# Beinhaltet alle Funktionen, die den Verlauf der Forschung über Zeit betreffen

createEvolutionPlot <- function(column,
                                data,
                                top = TRUE,
                                bottom = TRUE) {
  if (top) {
    if (bottom) {
      qplot(data[, "Publikationsjahr"],
            data[, paste0(column, "O")],
            data = data,) +
        geom_point(color = "#8a071d") +
        geom_smooth(color = "#8a071d",
                    fill = "#73484f") +
        geom_smooth(color = "#01106e",
                    fill = "#5f68a3",
                    aes(x = data[, "Publikationsjahr"], y = data[, paste0(column, "U")], )) +
        geom_point(color = "#01106e",
                   aes(x = data[, "Publikationsjahr"], y = data[, paste0(column, "U")], )) +
        theme(text=element_text(family="CMU Serif"))+
        labs(x = "Publikationsjahr",
             y = str_to_title(paste0(column, " Obere und Untere Grenze"), locale =
                                "de"))
    } else{
      createEvPlot(paste0(column, "O"), data, "#8a071d", "#73484f")
    }
  } else{
    if (bottom) {
      createEvPlot(paste0(column, "U"), data, "#01106e", "#5f68a3")
    } else
      return("ERROR")
  }
  
}

createEvPlot <- function(column,
                         data, col1, col2) {
  qplot(data[, "Publikationsjahr"],
        data[, column],
        data = data) +
    geom_point(color = col1) +
    geom_smooth(color = col1,
                fill = col2) +
    theme(text=element_text(family="CMU Serif"))+
    labs(x = "Publikationsjahr",
         y = str_to_title(column, locale =
                            "de"))
}
