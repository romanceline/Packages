#' Severity per country
#'
#' This is reading the excel file made of many sheets and returns a single long dataset for the selected country
#' @param infile,country
#' @keywords
#' @export
#' @examples
#' SeverityPerCountry()
#' Need dplyr, hablar and readxl packages


SeverityPerCountry<-function(infile,country){
  df <- read_excel(infile, sheet = country,col_names=FALSE) #DataFrame
  dft <- t(df) #Dataframe has become a character matrix due to transposition
  colnames(dft) <- dft[1,] #Rewriting the column names
  colnames(dft)<-make.names(colnames(dft)) #Make sure column names are in acceptable format, no special character, no space,...
  dft<-dft[-c(1), ] #Removing useless row
  Severity<-dft[,c("X1.4_IND","X4.1_IND","X4.2_IND","X4.3_IND","X4.5_IND","X5_IND","PHSM.SI","Date")] #Selects only fields we are interested in
  Severity<-as.data.frame(Severity) %>% mutate(Date=as.numeric(Date)) %>%
    mutate(Date=as.Date(Date,origin = "1899-12-30")) %>%
    convert(num("X1.4_IND","X4.1_IND","X4.2_IND","X4.3_IND","X4.5_IND","X5_IND","PHSM.SI"))
  #Retransforms the matrix in a dataframe where different types of data are allowed, converts characters into dates or numbers
  Severity$ADM0NAME<-country #Creates new field with country name as the final aim is to have a global dataset with all countries
  Severity<-Severity %>% select(
    Date,
    ADM0NAME,
    GlobalIndex=PHSM.SI,
    Masks= X1.4_IND,
    School = X4.1_IND,
    Workplace=X4.2_IND,
    Gatherings=X4.3_IND,
    StayHome=X4.5_IND,
    Travels=X5_IND,
  ) #Gives understandable column names
  return(Severity)
}


#' Severity for all countries
#'
#' This is reading the excel file made of many sheets and returns a single long dataset for all countries
#' @param infile
#' @keywords
#' @export
#' @examples
#' SeverityForAllCountries()

SeverityForAllCountries<-function(infile){
  ListCountries<-excel_sheets(infile)
  Severity<-data.frame()
  for (country in ListCountries){
    Severity_<-SeverityPerCountry(infile,country)
    Severity<-bind_rows(Severity,Severity_)
  }}