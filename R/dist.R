# Beinhaltet alle Funktionen, die die Verteilung und den Vergleich von zwei Zielattributen betreffen
createDist <- function(column, data) {
  if (length(data[, paste0(column, 'O')]) == 0) {
    return("ERROR")
  } else{
    res = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    for (i in 1:length(data[, paste0(column, 'O')])) {
      if (is.na(data[, paste0(column, 'U')][i])) {
        next
      }
      vW = data[, paste0(column, 'U')][i] * 10 + 1
      while (vW < data[, paste0(column, 'O')][i] * 10 + 1) {
        res[vW] = res[vW] + 1
        vW = vW + 1
      }
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
      ),
      value = res
    )
    return(result)
  }
}

createDistPlot <- function(column, data, rotate = FALSE,num=FALSE) {
  if(!num){
    if (rotate) {
      resData = createDist(column, data)
      return(
        ggplot(data = resData, aes(x = interval, y = value)) +
          geom_bar(stat = "identity") +
          labs(x = NULL, y = 'Anzahl Vorkommen') +
          theme(axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                text=element_text(family="CMU Serif")) +
          scale_fill_gradient(low = "#FFFF88", high = "#6e0101") +
          coord_flip()
      )
    } else{
      resData = createDist(column, data)
      return(
        ggplot(data = resData, aes(x = interval, y = value)) +
          geom_bar(stat = "identity") +
          labs(x = NULL, y = 'Anzahl Vorkommen') +
          theme(axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                text=element_text(family="CMU Serif"),)
      ) +
        scale_fill_gradient(low = "#FFFF88", high = "#6e0101")
    }
  }else{
    if (rotate) {
      resData = createDist(column, data)
      return(
        ggplot(data = resData, aes(x = interval, y = value)) +
          geom_bar(stat = "identity") +
          labs(x = NULL, y = 'Anzahl Vorkommen') +
          theme(axis.text.x = element_text(angle = 45,hjust=1),
                text=element_text(family="CMU Serif")) +
          scale_fill_gradient(low = "#FFFF88", high = "#6e0101") +
          coord_flip()
      )
    } else{
      resData = createDist(column, data)
      return(
        ggplot(data = resData, aes(x = interval, y = value)) +
          geom_bar(stat = "identity") +
          labs(x = NULL, y = 'Anzahl Vorkommen') +
          theme(axis.text.x = element_text(angle = 45,hjust=1),
                text=element_text(family="CMU Serif"),)
      ) +
        scale_fill_gradient(low = "#FFFF88", high = "#6e0101")
    }
  }
}

createDistMatrix <- function(column_1, column_2, data) {
  if (length(data[, paste0(column_1, 'O')]) == 0 ||
      length(data[, paste0(column_2, 'O')]) == 0) {
    return("ERROR")
  } else{
    res = matrix(0, 10, 10, byrow = TRUE)
    for (i in 1:length(data[, paste0(column_1, 'O')])) {
      if (is.na(data[, paste0(column_1, 'U')][i]) ||
          is.na(data[, paste0(column_2, 'U')][i])) {
        next
      }
      vW_1 = data[, paste0(column_1, 'U')][i] * 10 + 1
      while (vW_1 < data[, paste0(column_1, 'O')][i] * 10 + 1) {
        vW_2 = data[, paste0(column_2, 'U')][i] * 10 + 1
        while (vW_2 < data[, paste0(column_2, 'O')][i] * 10 + 1) {
          res[vW_1, vW_2] = res[vW_1, vW_2] + 1
          vW_2 = vW_2 + 1
        }
        vW_1 = vW_1 + 1
      }
    }
    print(paste0("1. Dimension:", column_1))
    print(paste0("2. Dimension:", column_2))
    return(res)
  }
}

createDistMatrixPlot <-
  function(column_1, column_2, data, numbers = TRUE) {
    varieties = c(
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
    pkgTest("reshape2")
    library(reshape2)
    pkgTest("ggplot2")
    library(ggplot2)
    df <-
      data.frame(id = varieties, createDistMatrix(column_1, column_2, data))
    colnames(df)[2:ncol(df)] <- varieties
    gg <- melt(df, id = "id")
    if (numbers) {
      return(
        ggplot(gg, aes(
          x = id, y = variable, fill = value
        )) +
          geom_tile(show.legend = FALSE) +
          geom_text(aes(label = round(value, 2),family="CMU Serif"), alpha = 0.6) +
          scale_fill_gradient(low = "#FFFF88", high = "#FF0000") +
          labs(
            x = str_to_title(column_1, locale = "de"),
            y = str_to_title(column_2, locale = "de")
          ) +
          theme(legend.key = element_blank(),text=element_text(family="CMU Serif"),axis.text.x = element_text(angle = 45,hjust=1))
      )
    }
    else{
      return(
        ggplot(gg, aes(
          x = id, y = variable, fill = value
        )) +
          geom_tile(show.legend = FALSE) +
          scale_fill_gradient(low = "#FFFF88", high = "#FF0000") +
          labs(
            x = str_to_title(column_1, locale = "de"),
            y = str_to_title(column_2, locale = "de")
          ) +
          theme(
            legend.key = element_blank(),
            text=element_text(family="CMU Serif"),
            axis.text.x = element_text(angle = 45,hjust=1)
          )
      )
    }
    
  }

createDistAndMatrix <-
  function(column_1, column_2, data, numbers = TRUE) {
    pkgTest("patchwork")
    library(patchwork)
    pkgTest("ggpubr")
    library(ggpubr)
    mat = createDistMatrixPlot(column_1, column_2, data, numbers)
    dist1 = createDistPlot(column_1, data, FALSE)
    dist2 = createDistPlot(column_2, data, TRUE)
    empty = ggplot() + theme_void()
    dist1 + empty + mat + dist2 + plot_layout(
      ncol = 2,
      nrow = 2,
      widths = c(4, 1.4),
      heights = c(1.4, 4)
    )
  }