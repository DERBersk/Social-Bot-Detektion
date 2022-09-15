# Funktion zur Erstellung einer Darstellung zur Visualisierung der Clusteranalyse von Gruppenbasierten und Gegnerbasierten Ansätzen
clusterPlot <- function(data,Group=TRUE) {
  if(Group){
    ggplot(
      data = ex_data,
      aes(
        x=NetzwerkgroesseU,
        y=NetzwerkgroesseO,
        color=group
      )
    )+geom_point()+
      geom_mark_hull(concavity = 5,expand=0,radius=0,aes(fill=group))+
      theme(text = element_text(family="CMU Serif"))+
      labs(color="Gruppenbasiert",fill="Gruppenbasiert")
  }else{
    ggplot(
      data = ex_data,
      aes(
        x=NeuheitU,
        y=NeuheitO,
        color=adversarial
      )
    )+geom_point()+
      geom_mark_hull(concavity = 5,expand=0,radius=0,aes(fill=adversarial))+
      theme(text = element_text(family="CMU Serif"))+
      labs(color="Gegnerbasiert",fill="Gegnerbasiert")
  }
}

relativeYearColumn <- function(column){
  column=column-2010
  column=column/12
  return(column)
}

clusterOutput <- function(data,clustercount) {
  km <- kmeans(as.matrix(na.omit(data)), clustercount)
  return(km)
}