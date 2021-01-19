#' SpaguettiPlot function
#' This function will highlight one variable (NewDeaths, NewCases, CasesIncidence, DeathsIncidence) for one country against other countries
#' @param Country - Country to highlight
#' @param Variable - Variable can be NewCases,NewDeaths,NewCases_Rate,NewDeaths_Rate (will be developed further with other variables)
#' @param AllOrSelection - Use 'All' if you want to compare the selected country to all other countries in EURO Region, use 'c(CountryA,CountryB,CountryC,...)' if you want to compare to some countries only
#' @param color - Color to use to highlight the country Ex: '#000000'
#' @export

SpaguettiPlot<-function (Country,Variable,AllOrSelection,ListCountriesToCompare,col){
  AllCountries<-Population$ADM0NAME

  Dataset<-BuildExtendedEpiDataset(Country)

  GlobalDataset<-data.frame()
  if (AllOrSelection=='All'){
    ListCountriesToCompare<-AllCountries
  }

  for (ctr in ListCountriesToCompare){
    GlobalDataset_<-BuildExtendedEpiDataset(ctr)
    GlobalDataset<-bind_rows(GlobalDataset_,GlobalDataset)
  }

  if (Variable=='NewCases'){
    Dataset_<-Dataset %>% select(ADM0NAME,DateReport,y_value=Spline_3DaysAverageCases)
    GlobalDataset<-GlobalDataset %>% select(ADM0NAME,DateReport,y_value=Spline_3DaysAverageCases)
  }

  if (Variable=='NewDeaths'){
    Dataset_<-Dataset %>% select(ADM0NAME,DateReport,y_value=Spline_3DaysAverageDeaths)
    GlobalDataset<-GlobalDataset %>% select(ADM0NAME,DateReport,y_value=Spline_3DaysAverageDeaths)
  }

  if (Variable=='NewCases_Rate'){
    Dataset_<-Dataset %>% select(ADM0NAME,DateReport,y_value=Spline_3DaysAverageCases_Rate)
    GlobalDataset<-GlobalDataset %>% select(ADM0NAME,DateReport,y_value=Spline_3DaysAverageCases_Rate)
  }

  if (Variable=='NewDeaths_Rate'){
    Dataset_<-Dataset %>% select(ADM0NAME,DateReport,y_value=Spline_3DaysAverageDeaths_Rate)
    GlobalDataset<-GlobalDataset %>% select(ADM0NAME,DateReport,y_value=Spline_3DaysAverageDeaths_Rate)
  }




  if (grepl("Cases", Variable))
  {if (Variable=="NewCases") {labelY<-"Daily number of cases"}
  if(Variable=="NewCases_Rate") {labelY<-"Daily rate of cases (per 100.000)"}}

  if (grepl("Deaths", Variable))
  {if (Variable=="NewDeaths") {labelY<-"Daily number of deaths"}
  if (Variable=="NewDeaths_Rate") {labelY<-"Daily rate of deaths (per one million)"}}



  plot<-ggplot()+
    geom_line(data=GlobalDataset,aes(x=DateReport,y=y_value,group=ADM0NAME),color="grey")+
    geom_line(data=Dataset_,
              aes(x=DateReport,y=y_value),
              color=col,size=0.8)+
    labs(x="Date",y=labelY)+
    theme_minimal()
  return(plot=plot)
}
