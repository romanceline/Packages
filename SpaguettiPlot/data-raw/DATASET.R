## code to prepare `DATASET` dataset goes here
Population<-read.csv('C:/Users/romanc/Documents/GitHub/Packages/ref_Country.csv')
usethis::use_data(Population, overwrite = TRUE)

setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
