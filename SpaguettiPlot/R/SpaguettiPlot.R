#' SpaguettiPlot function
#' This function will highlight one variable (NewDeats, NewCases, CasesIncidence, DeathsIncidence) for one country against other countries
#' @param Country - Country to highlight
#' @param Variable - Variable can be NewCases,NewDeaths,TotalCases,TotalDeaths, DailyRateCases, DailyRateDeaths, CumulativeRateCases,CumulativeRateDeaths
#' @param AllOrSelection - Use 'All' if you want to compare the selected country to all other countries in EURO Region, use 'c(CountryA,CountryB,CountryC,...)' if you want to compare to some countries only
#'
#' @param color - Color to use to highlight the country Ex: '#000000'
#' @export

SpaguettiPlot<-function (Country,Variable,AllOrSelection,ListCountriesToCompare,col){
  Population<-Population %>% mutate(ADM0NAME=str_to_title(ADM0NAME)) %>% select(ADM0NAME,UNPOP2019)

  Dataset<-GetEpiData() %>% mutate(ADM0NAME=str_to_title(ADM0NAME)) %>%
    left_join(Population, by='ADM0NAME') %>%
    mutate(DailyRateCases=NewCases/UNPOP2019*100000,
           DailyRateDeaths=NewDeaths/UNPOP2019*100000,
           CumulativeRateCases=TotalCases/UNPOP2019*1000000,
           CumulativeRateDeaths=TotalDeaths/UNPOP2019*1000000)

  Variable<-sym(Variable)

  if(AllOrSelection=="All"){
    ComparativeDataset<-Dataset
  }

  if(AllOrSelection=="Selection"){
    ComparativeDataset <- Dataset %>% filter (ADM0NAME %in% ListCountriesToCompare)
  }

  if (grepl("Cases", Variable))
  {if (Variable=="NewCases") {labelY<-"Daily number of cases"}
  else if (Variable=="TotalCases") {labelY<-"Total number of cases"}
  else if (Variable=="DailyRateCases") {labelY<-"Daily rate of cases (per 100.000)"}
  else if (Variable=="CumulativeRateCases") {labelY<-"Cumulative rate of cases (per 100.000)"}}

  else if (grepl("Deaths", Variable))
  {if (Variable=="NewDeaths") {labelY<-"Daily number of deaths"}
  else if (Variable=="TotalDeaths") {labelY<-"Total number of deaths"}
  else if (Variable=="DailyRateDeaths") {labelY<-"Daily rate of deaths (per one million)"}
  else if (Variable=="CumulativeRateDeaths") {labelY<-"Cumulative rate of deaths (per one million)"}}


  CountryDataset<-Dataset %>% filter (ADM0NAME==Country)

  plot<-ggplot()+
    geom_line(data=ComparativeDataset,aes(x=DateReport,y=!!Variable,group=ADM0NAME),color="grey")+
    geom_line(data=CountryDataset,
              aes(x=DateReport,y=!!Variable),
              color=col,size=0.8)+
    labs(x="Date",y=labelY)+
    theme_minimal()
  return(list(plot=plot))
}
