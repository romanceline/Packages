## code to prepare `DATASET` dataset goes here
PopulationData<-read.csv('C:/Users/romanc/Documents/GitHub/Packages/ref_Country.csv')
usethis::use_data(PopulationData, overwrite = TRUE)

#setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
