# Funktion zur Darstellung der Publikationen über Zeit
timeAnalysis <- function(data) {
  resTable <- table(data[, "Publikationsjahr"])
  resData <-
    data.frame(word = as.Date(names(resTable), format = "%Y-%m-%d"),
               freq = as.numeric(resTable))
  ggplot(data = resData,
         aes(x = word,
             y = freq)) +
    geom_col(aes(x = word, y = freq), alpha = 0.6) +
    geom_smooth(color = "#8a071d",se=FALSE,span=0.4) +
    theme(text=element_text(family="CMU Serif"))+
    labs(x = "Jahr",
         y = "Anzahl der Publikationen")
}

# Funktion zur Darstellung der verschiedenen technischen Ansätze über Zeit
timeAnalysisOfNNAnsatz <- function(data) {
  resTable <- table(data[, "Publikationsjahr"], data[, "nnAnsatz"])
  resData = data.frame(
    dates = row.names(resTable),
    unsupervised = resTable[, 1],
    semisupervised = resTable[, 2],
    supervised = resTable[, 3],
    heuristic = resTable[, 4]
  )
  ggplot(aes(
    x = as.Date(dates, format = "%Y-%m-%d"),
    y = unsupervised,
    color = "unueberwacht"
  ),
  data = resData,) +
    geom_line() +
    geom_line(aes(
      x = as.Date(dates, format = "%Y-%m-%d"),
      y = semisupervised,
      color = "semi-ueberwacht"
    )) +
    geom_line(aes(
      x = as.Date(dates, format = "%Y-%m-%d"),
      y = supervised,
      color = "ueberwacht"
    )) +
    geom_line(aes(
      x = as.Date(dates, format = "%Y-%m-%d"),
      y = heuristic,
      color = "andere"
    )) +
    theme(text=element_text(family="CMU Serif"))+
    labs(x = "Jahr",
         y = "Anzahl der Publikationen",
         color = "Legende")
}

# Funktion zur Darstellung der Entwicklung der Steuerungsorientierung über Zeit
timeAnalysisSteuerung <- function(data,rel=FALSE) {
  resTable <-
    table(is.na(data[, "SteuerungO"]), data[, "Publikationsjahr"])
  resData <- data.frame(
    word = as.Date(colnames(resTable), format = "%Y-%m-%d"),
    "nicht Steuerungsorientiert" = resTable[2, ],
    Steuerungsorientiert = resTable[1, ]
  )
  resData <- melt(resData ,  id.vars = 'word', variable.name = 'series')
  if(rel){
    ggplot(data = resData,
           aes(x = word,
               y = value)) +
      geom_bar(alpha = 0.6, aes(fill = resData[,"series"]),stat="identity",position="fill") +
      scale_fill_manual(values=c("#999999", "#E69F00"))+
      theme(text=element_text(family="CMU Serif"))+
      labs(x = "Jahr",
           y = "Anzahl der Publikationen",
           fill="Legende")
  } else{
    ggplot(data = resData,
           aes(x = word,
               y = value)) +
      geom_bar(alpha = 0.6, aes(fill = resData[,"series"]),stat="identity") +
      scale_fill_manual(values=c("#999999", "#E69F00"))+
      theme(text=element_text(family="CMU Serif"))+
      labs(x = "Jahr",
           y = "Anzahl der Publikationen",
           fill="Legende")
  }
}

# Funktion zur Darstellung der Entwicklung der Gruppenbasierung über Zeit
timeAnalysisGroup <- function(data,rel=FALSE) {
  resTable <-
    table(data[, "group"], data[, "Publikationsjahr"])
  resData <- data.frame(
    word = as.Date(colnames(resTable), format = "%Y-%m-%d"),
    individuell= resTable[1, ],
    gruppenbasiert = resTable[2, ]
  )
  resData <- melt(resData ,  id.vars = 'word', variable.name = 'series')
  if(rel){
    ggplot(data = resData,
           aes(x = word,
               y = value)) +
      geom_bar(alpha = 0.6, aes(fill = resData[,"series"]),stat="identity",position="fill") +
      scale_fill_manual(values=c("#999999","#E69F00"))+
      theme(text=element_text(family="CMU Serif"))+
      labs(x = "Jahr",
           y = "Anzahl der Publikationen",
           fill="Legende")
  } else{
    ggplot(data = resData,
           aes(x = word,
               y = value)) +
      geom_bar(alpha = 0.6, aes(fill = resData[,"series"]),stat="identity") +
      scale_fill_manual(values=c("#999999","#E69F00"))+
      theme(text=element_text(family="CMU Serif"))+
      labs(x = "Jahr",
           y = "Anzahl der Publikationen",
           fill="Legende")
  }
}

# Funktion zur Darstellung der Entwicklung der Gegnerbasierung über Zeit
timeAnalysisAdversarial <- function(data,rel=FALSE) {
  resTable <-
    table(data[, "adversarial"], data[, "Publikationsjahr"])
  resData <- data.frame(
    word = as.Date(colnames(resTable), format = "%Y-%m-%d"),
    klassisch = resTable[1, ],
    gegnerbasiert = resTable[2, ]
  )
  resData <- melt(resData ,  id.vars = 'word', variable.name = 'series')
  if(rel){
    ggplot(data = resData,
           aes(x = word,
               y = value)) +
      geom_bar(alpha = 0.6, aes(fill = resData[,"series"]),stat="identity",position="fill") +
      scale_fill_manual(values=c("#999999", "#E69F00"))+
      theme(text=element_text(family="CMU Serif"))+
      labs(x = "Jahr",
           y = "Anzahl der Publikationen",
           fill="Legende")
  }else{
    ggplot(data = resData,
           aes(x = word,
               y = value)) +
      geom_bar(alpha = 0.6, aes(fill = resData[,"series"]),stat="identity") +
      scale_fill_manual(values=c("#999999", "#E69F00"))+
      theme(text=element_text(family="CMU Serif"))+
      labs(x = "Jahr",
           y = "Anzahl der Publikationen",
           fill="Legende")
  }
  
}