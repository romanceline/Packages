## code to prepare `DATASET` dataset goes here
WHO_ref<-read.csv('C:/Users/romanc/Documents/GitHub/Packages/BuildExtendedEpiDataset/data-raw/ref_Country.csv')
usethis::use_data(WHO_ref, overwrite = TRUE)
