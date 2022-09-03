prepData <- function(data) {
  for (i in 1:length(data$NeuheitO)) {
    if (data$NeuheitO[i] == "NULL") {
      data$NeuheitO[i] = NA
      data$NeuheitU[i] = NA
    }
    if (data$SteuerungO[i] == "NULL") {
      data$SteuerungO[i] = NA
      data$SteuerungU[i] = NA
    }
  }
  
  data$Publikationsjahr = lubridate::ymd(data$Publikationsjahr, truncated =
                                           2L)
  
  data$NeuheitO = gsub(",", ".", data$NeuheitO)
  data$NeuheitU = gsub(",", ".", data$NeuheitU)
  data$SteuerungO = gsub(",", ".", data$SteuerungO)
  data$SteuerungU = gsub(",", ".", data$SteuerungU)
  
  data$NeuheitO = as.numeric(data$NeuheitO)
  data$NeuheitU = as.numeric(data$NeuheitU)
  data$SteuerungO = as.numeric(data$SteuerungO)
  data$SteuerungU = as.numeric(data$SteuerungU)
  
  # Clusteranalyse
  #wenn obergrenze über 0,4, neues Cluster: Clusteranalyse als weg zur Klassifizierung, ob gruppenbasiert oder nicht
  data$group=clusterOutput(data[, c("NetzwerkgroesseU","NetzwerkgroesseO")],3)$cluster
  data$group=data$group==data$group[60]
  
  # Ohne Publikationsjahr zeigen sich vier klassen
  data$adversarial=clusterOutput(data[, c("NeuheitO","NeuheitU")],4)$cluster
  data$adversarial=data$adversarial==data$adversarial[31]
  
  return(data)
}
