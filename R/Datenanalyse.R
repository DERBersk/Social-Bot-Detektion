# Package Installation und Aktivierung

# Muss angepasst werden!
setwd("C:/PATH_TO_FOLDER/R")

pkgTest <- function(x)
{
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if (!require(x, character.only = TRUE))
      stop("Package not found")
  }
}

pkgTest("wordcloud")
library(wordcloud)
pkgTest("RColorBrewer")
library(RColorBrewer)
pkgTest("wordcloud2")
library(wordcloud2)
pkgTest("lubridate")
library(lubridate)
pkgTest("ggplot2")
library(ggplot2)
pkgTest("stringr")
library(stringr)
pkgTest("extrafont")
library(extrafont)
pkgTest("remotes")
library(remotes)
pkgTest("reshape2")
library(reshape2)
pkgTest('stats')
require(graphics)
pkgTest("factoextra")
library(factoextra)
pkgTest("ggforce")
library(ggforce)

remotes::install_version("Rttf2pt1", version = "1.3.8")
# Muss angepasst werden!
font_import(paths = "C:/Users/USERNAME/AppData/Local/Microsoft/Windows/Fonts")

# Laden von Modern Computer (Installation im Paket erklärt)
extrafont::loadfonts()

# Datenfetch
ex_data <-
  read.table(
    file = "../titles-list.csv",
    sep = ";",
    dec = ",",
    header = TRUE
  )

# Parsen aller Funktionsdateien
source("seperateByComma.R")
source("lowerUpper.R")
source("dist.R")
source("dataPrep.R")
source("evolution.R")
source("wordcloud.R")
source("timeAnalysis.R")
source("featureToNN.R")
source("clusterAnalyse.R")
source("correlationPlot.R")

# Vorbereiten der Daten ("NULL" zu NA)
ex_data = prepData(ex_data)

################
#ARBEITSBEREICH#
################

# Verteilungen der Dimensionen
# TODO: relative Verteilungen
createDistAndMatrix("Zielrichtung", "Imitation", ex_data, numbers = TRUE)
createDistAndMatrix("Zielrichtung", "Steuerung", ex_data, numbers = TRUE)
createDistAndMatrix("Zielrichtung", "Neuheit", ex_data, numbers = TRUE)
createDistAndMatrix("Zielrichtung", "Netzwerkgroesse", ex_data, numbers = TRUE)
createDistAndMatrix("Imitation", "Steuerung", ex_data, numbers = TRUE)
createDistAndMatrix("Imitation", "Neuheit", ex_data, numbers = TRUE)
createDistAndMatrix("Imitation", "Netzwerkgroesse", ex_data, numbers = TRUE)
createDistAndMatrix("Steuerung", "Neuheit", ex_data, numbers = TRUE)
createDistAndMatrix("Steuerung", "Netzwerkgroesse", ex_data, numbers = TRUE)

createDistAndMatrix("Neuheit", "Netzwerkgroesse", ex_data[c(5,74),], numbers = TRUE)
createDistPlot("Steuerung", ex_data,num=TRUE)

# Boxplot jedes Jahr
createLowerUpperBP("Netzwerkgroesse", ex_data)

# EntwicklungPlot
createEvolutionPlot("Imitation", ex_data)
createEvolutionPlot("Netzwerkgroesse", ex_data, TRUE, FALSE)

# NNAnsatz Analyse
createFeatureToNNAnsatzMatrix("Imitation", ex_data)
# Zeitanalysen
timeAnalysis(ex_data)
timeAnalysisOfNNAnsatz(ex_data)
timeAnalysisSteuerung(ex_data)
timeAnalysisAdversarial(ex_data)
timeAnalysisGroup(ex_data)
# Wordclouds
createWC("Target.Group", ex_data)

createWC("SNS",ex_data)

timeAnalysisSteuerung(ex_data)
timeAnalysisSteuerung(ex_data,TRUE)
# Clusteranalyse
#wenn obergrenze über 0,4, neues Cluster: Clusteranalyse als weg zur Klassifizierung, ob gruppenbasiert oder nicht
fviz_nbclust(as.matrix(na.omit(ex_data[, c("NetzwerkgroesseU","NetzwerkgroesseO")])), kmeans, method = "wss")
ex_data$group=clusterOutput(ex_data[, c("NetzwerkgroesseU","NetzwerkgroesseO")],2)$cluster

ex_data$group=ex_data$group==1

clusterPlot(ex_data)
# Ohne Publikationsjahr zeigen sich vier klassen
fviz_nbclust(as.matrix(na.omit(ex_data[, c("NeuheitO","NeuheitU")])), kmeans, method = "wss")
ex_data$adversarial=clusterOutput(ex_data[, c("NeuheitO","NeuheitU")],4)$cluster

ex_data$adversarial=ex_data$adversarial==4

clusterPlot(ex_data,FALSE)

createCorPlot(ex_data)
#####################
#Ende Arbeitsbereich#
#####################

