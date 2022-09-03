createExampleDistributionZielrichtung <- function() {
  library(reshape2)
  library(ggplot2)
  df <- data.frame(
    interval = c(
      "0 - 0,1",
      "0,1 - 0,2",
      "0,2 - 0,3",
      "0,3 - 0,4",
      "0,4 - 0,5",
      "0,5 - 0,6",
      "0,6 - 0,7",
      "0,7 - 0,8",
      "0,8 - 0,9",
      "0,9 - 1"
    ),
    "Video-Spiel-Bot"=c(1,1,1,1,1,1,0,0,0,0),
    "Politisches Botnetz"=c(0,0,0,0,0,1,1,1,1,1)
  )
  
  gg <- melt(df, id = "interval")
  return(
    ggplot(gg, aes(
      y = variable, x = interval, fill = value
    )) +
      geom_tile(show.legend = FALSE) +
      geom_text(aes(family="CMU Serif",label = round(value, 1)), alpha = 0.6) +
      scale_fill_gradient(low = "#FFF2CC", high = "#F8CECC") +
      labs(y = "Botarten", x = "Auftreten der Dimension") +
      theme(legend.key = element_blank(),
            text=element_text(family="CMU Serif",),
            axis.text.x = element_text(angle = 45,hjust=1))
  )
}

generateZielrichtung <- function(){
  cairo_pdf("RZielrichtung.pdf", height=2, width=8)
  
  createExampleDistributionZielrichtung()
  
  dev.off()
}

createExampleDistributionImitation <- function() {
  library(reshape2)
  library(ggplot2)
  df <- data.frame(
    interval = c(
      "0 - 0,1",
      "0,1 - 0,2",
      "0,2 - 0,3",
      "0,3 - 0,4",
      "0,4 - 0,5",
      "0,5 - 0,6",
      "0,6 - 0,7",
      "0,7 - 0,8",
      "0,8 - 0,9",
      "0,9 - 1"
    ),
    "Nachrichtenbot"=c(1,1,1,1,0,0,0,0,0,0),
    "Chatbot"=c(0,0,1,1,1,1,1,1,1,1)
  )
  gg <- melt(df, id = "interval")
  return(
    ggplot(gg, aes(
      y = variable, x = interval, fill = value
    )) +
      geom_tile(show.legend = FALSE) +
      geom_text(aes(family="CMU Serif",label = round(value, 1)), alpha = 0.6) +
      scale_fill_gradient(low = "#FFF2CC", high = "#F8CECC") +
      labs(y = "Botarten", x = "Auftreten der Dimension") +
      theme(legend.key = element_blank(),
            text=element_text(family="CMU Serif",),
            axis.text.x = element_text(angle = 45,hjust=1))
  )
}

generateImitation <- function(){
  cairo_pdf("RImitation.pdf", height=2, width=8)
  
  createExampleDistributionImitation()
  
  dev.off() 
}

createExampleDistributionSteuerung <- function() {
  library(reshape2)
  library(ggplot2)
  df <- data.frame(
    interval = c(
      "0 - 0,1",
      "0,1 - 0,2",
      "0,2 - 0,3",
      "0,3 - 0,4",
      "0,4 - 0,5",
      "0,5 - 0,6",
      "0,6 - 0,7",
      "0,7 - 0,8",
      "0,8 - 0,9",
      "0,9 - 1"
    ),
    "Hybrider Bot"=c(0,0,0,1,1,1,1,0,0,0),
    "Automatisiertes Botnetz"=c(0,0,0,0,1,1,1,1,1,1)
  )
  gg <- melt(df, id = "interval")
  return(
    ggplot(gg, aes(
      y = variable, x = interval, fill = value
    )) +
      geom_tile(show.legend = FALSE) +
      geom_text(aes(family="CMU Serif",label = round(value, 1)), alpha = 0.6) +
      scale_fill_gradient(low = "#FFF2CC", high = "#F8CECC") +
      labs(y = "Botarten", x = "Auftreten der Dimension") +
      theme(legend.key = element_blank(),
            text=element_text(family="CMU Serif",),
            axis.text.x = element_text(angle = 45,hjust=1))
  )
}

generateSteuerung <- function(){
  cairo_pdf("RSteuerung.pdf", height=2, width=8)
  
  createExampleDistributionSteuerung()
  
  dev.off()
}

createExampleDistributionNeuheit <- function() {
  library(reshape2)
  library(ggplot2)
  df <- data.frame(
    interval = c(
      "0 - 0,1",
      "0,1 - 0,2",
      "0,2 - 0,3",
      "0,3 - 0,4",
      "0,4 - 0,5",
      "0,5 - 0,6",
      "0,6 - 0,7",
      "0,7 - 0,8",
      "0,8 - 0,9",
      "0,9 - 1"
    ),
    "Generischer Spambot"=c(1,1,1,1,0,0,0,0,0,0),
    "Politisches Botnetz"=c(0,0,0,1,1,1,1,1,1,1)
  )
  gg <- melt(df, id = "interval")
  return(
    ggplot(gg, aes(
      y = variable, x = interval, fill = value
    )) +
      geom_tile(show.legend = FALSE) +
      geom_text(aes(family="CMU Serif",label = round(value, 1)), alpha = 0.6) +
      scale_fill_gradient(low = "#FFF2CC", high = "#F8CECC") +
      labs(y = "Botarten", x = "Auftreten der Dimension") +
      theme(legend.key = element_blank(),
            text=element_text(family="CMU Serif",),
            axis.text.x = element_text(angle = 45,hjust=1))
  )
}

generateNeuheit <- function(){
  cairo_pdf("RNeuheit.pdf", height=2, width=8)
  
  createExampleDistributionNeuheit()
  
  dev.off()
}

createExampleDistributionNetzwerk <- function() {
  library(reshape2)
  library(ggplot2)
  df <- data.frame(
    interval = c(
      "0 - 0,1",
      "0,1 - 0,2",
      "0,2 - 0,3",
      "0,3 - 0,4",
      "0,4 - 0,5",
      "0,5 - 0,6",
      "0,6 - 0,7",
      "0,7 - 0,8",
      "0,8 - 0,9",
      "0,9 - 1"
    ),
    "Nachrichtenbot"=c(1,0,0,0,0,0,0,0,0,0),
    "Gerüchtebot"=c(1,1,1,1,1,1,1,0,0,0)
  )
  gg <- melt(df, id = "interval")
  return(
    ggplot(gg, aes(
      y = variable, x = interval, fill = value
    )) +
      geom_tile(show.legend = FALSE) +
      geom_text(aes(family="CMU Serif",label = round(value, 1)), alpha = 0.6) +
      scale_fill_gradient(low = "#FFF2CC", high = "#F8CECC") +
      labs(y = "Botarten", x = "Auftreten der Dimension") +
      theme(legend.key = element_blank(),
            text=element_text(family="CMU Serif",),
            axis.text.x = element_text(angle = 45,hjust=1))
  )
}

generateNetzwerk <- function(){
  cairo_pdf("RNetzwerk.pdf", height=2, width=8)
  
  createExampleDistributionNetzwerk()
  
  dev.off()
}
