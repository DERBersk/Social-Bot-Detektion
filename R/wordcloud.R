# Function erstellt eine Wordcloud mit entweder der Spalte Target.Group oder SNS.
createWC <- function(column, data, dark = FALSE) {
  if (column == "Target.Group" || column == "SNS") {
    Target = table(seperateByComma(data[, column]))
    wordC <- data.frame(word = names(Target),
                        freq = as.numeric(Target))
    
    if (dark) {
      par(bg = "black")
      wordcloud(
        words = wordC$word,
        freq = wordC$freq,
        min.freq = 1,
        random.order = FALSE,
        colors = terrain.colors(length(wordC$word),alpha = 0.9),
        family="CMU Serif"
      )
    } else{
      par(bg = "white")
      wordcloud(
        words = wordC$word,
        freq = wordC$freq,
        min.freq = 1,
        random.order = FALSE,
        colors = brewer.pal(6, "Dark2"),
        family="CMU Serif"
      )
    }
  } else{
    return("ERROR")
  }
}
