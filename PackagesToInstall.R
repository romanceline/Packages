#Packages needed to use all of these packages
#Some custom packages make use of other custom packages, important to have them all downloaded

Packages_CRAN<-c('ggplot2','hablar','devtools','readxl','zoo','dplyr','tidyr','httr','jsonlite','lubridate','cowplot','stringr','stats','imputeTS')
Packages_Custom<-c('WHOCountryNames','RtChart','SpaguettiPlot','ReadSeverityExcel','GetEpiData','BuildExtendedEpiDataset','PHSMChart')

for (i in Packages_CRAN) {
  print(i)
  if (!i %in% installed.packages())
  {install.packages(i)}
  library(i, character.only = TRUE)
}

for (i in Packages_Custom) {
  print(i)
  if (!i %in% installed.packages())
  {install_github(paste0('romanceline/Packages/',i))}
  library(i, character.only = TRUE)
}