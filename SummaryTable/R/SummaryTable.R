#' SummaryTable
#' Function giving the summary table
#'
#' @param dataset qry_COVID_cases_by_date_final.csv
#'
#' @export
#' @examples
#' @SummaryTable()

SummaryTable<-function(dataset){

  EuroDataset<-GetEpiData() %>%
    mutate(DateReport=as.Date(DateReport),ADM0NAME=str_to_title(ADM0NAME))

  Dataset_Epiforecast_ <- read.csv("https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/national/cases/summary/rt.csv") %>%
    filter(type=='estimate')

  Rt_country<-function(ctr){
    Dataset_Epiforecast_Country<-Dataset_Epiforecast_ %>% filter(country==ctr)
    MaxDate<-max(Dataset_Epiforecast_Country$date)
    DataMaxDate<-Dataset_Epiforecast_Country %>% filter(date==MaxDate)
    return(DataMaxDate)
  }

  GlobalRtDataset<-data.frame()
  for (ctr in unique(Dataset_Epiforecast_$country)){
    GlobalRtDataset_<-Rt_country(ctr)
    GlobalRtDataset<-bind_rows(GlobalRtDataset,GlobalRtDataset_)
  }

  Dataset_Epiforecast <- GlobalRtDataset %>%
    select(ADM0NAME="country", Last_Update='date',RO='median',CI_low='lower_90', CI_up='upper_90') %>%
    mutate(Trend=if_else(CI_low<1 & CI_up<1,"Decreasing",if_else(CI_low<=1 & CI_up>=1,"Stable","Increasing"))) %>%
    WHOCountryNames(ADM0NAME)


  Dataset_GlobalPopulation <- GetPopulation() %>% rename(ADM0NAME=ADM0_NAME)

  #This will need to change at some point
  Dataset_LastReportingDate<-data.frame(ADM0NAME=unique(EuroDataset$ADM0NAME),
                                      LastUpdate=max(EuroDataset$DateReport))#ToCorrectTomorrow


  Dataset_TransmissionScenario<-dataset %>%
    select(ADM0NAME,TransmissionStatus=STATUS) %>%
    mutate(ADM0NAME=str_to_title(ADM0NAME),TransmissionStatus=str_to_title(TransmissionStatus),
           TransmissionStatus_FullName=if_else(TransmissionStatus=="Clusters","Clusters of cases",
                                               if_else(TransmissionStatus=="Community","Community Transmission",
                                                       if_else(TransmissionStatus=="Sporadic","Sporadic cases","Pending"))))

  MainDataset<-EuroDataset %>%
    left_join(Dataset_Epiforecast,by="ADM0NAME") %>%
    left_join(Dataset_GlobalPopulation,by="ADM0NAME") %>%
    left_join(Dataset_TransmissionScenario,by="ADM0NAME")

# Preparing number formatting
plain <- function(x,...) {
  format(x, ..., scientific = FALSE, trim = TRUE,big.mark=" ")
}

CurrentDate<-max(EuroDataset$DateReport)

# NbCases_Days('France',14) would give the number of cases in France over the last 14 days (starting from last reporting date)
NbCases_Days<-function(Country,Days){
  LastUpdate<-(Dataset_LastReportingDate %>% filter(ADM0NAME==Country))$LastUpdate
  if (CurrentDate-LastUpdate<=3){
    Cases<-(((MainDataset %>% filter(ADM0NAME==Country,DateReport==LastUpdate))$TotalCases-
               (MainDataset %>% filter(ADM0NAME==Country,DateReport==LastUpdate-Days))$TotalCases))}
  else {
    Cases<-NA
  }
  return(Cases)
}

TableCases<-data.frame()

for (ctr in unique(MainDataset$ADM0NAME)){
  Cases_7Days<-NbCases_Days(ctr,7)
  vector<-data.frame(ctr,Cases_7Days)
  TableCases<-bind_rows(vector,TableCases)
}






# Function that calculates a certain incidence with a certain delay
# Ex:
# - Incidence('France',14,0) will give the latest 14 days incidence in France
# - Incidence('Kosovo',7,7) will give the 7 days incidence in Kosovo one week before last reporting date.
# The function takes into account potential reporting delays.
# If the delay is more than 3 days, will return NA.
Incidence<-function(country,incidence,delay){
  LastUpdate<-(Dataset_LastReportingDate %>% filter(ADM0NAME==country))$LastUpdate
  if (CurrentDate-LastUpdate<=3){
    Pop<-(Dataset_GlobalPopulation %>% filter(ADM0NAME==country))$Population
    Incidence<-round(((MainDataset %>% filter(ADM0NAME==country) %>% filter(DateReport==LastUpdate-delay))$TotalCases-
                        (MainDataset %>% filter(ADM0NAME==country) %>% filter(DateReport==LastUpdate-incidence-delay))$TotalCases)/Pop*100000,1)}
  else {
    Incidence<-NA
  }
  return(Incidence)
}

#Creates table all countries and their incidences
TableIncidences<-data.frame()
for (ctr in unique(MainDataset$ADM0NAME)){
  FrtDaysIncidence_Latest<-Incidence(ctr,14,0)
  FrtDaysIncidence_1WkEarlier<-Incidence(ctr,14,7)
  FrtDaysIncidence_2WkEarlier<-Incidence(ctr,14,14)
  SvnDaysIncidence_Latest<-Incidence(ctr,7,0)
  SvnDaysIncidence_1WkEarlier<-Incidence(ctr,7,7)
  SvnDaysIncidence_2WkEarlier<-Incidence(ctr,7,14)
  vector<-data.frame(ctr,
                     FrtDaysIncidence_Latest,
                     FrtDaysIncidence_1WkEarlier,
                     FrtDaysIncidence_2WkEarlier,
                     SvnDaysIncidence_Latest,
                     SvnDaysIncidence_1WkEarlier,
                     SvnDaysIncidence_2WkEarlier
  )
  TableIncidences<-bind_rows(vector,TableIncidences)
}

#Adds a field that calculates the change in 14days incidence over 2 weeks
TableIncidences<-TableIncidences %>%
  mutate(Change14DaysIncidence_2Weeks=round(FrtDaysIncidence_Latest/FrtDaysIncidence_2WkEarlier*100-100,1)) %>%
  mutate(Change7DaysIncidence_1week=round(SvnDaysIncidence_Latest/SvnDaysIncidence_1WkEarlier*100-100,1))



SummaryTable_AllCountries<- MainDataset %>% filter(DateReport==CurrentDate) %>%
  select(ADM0NAME,TotalCases,TotalDeaths,TransmissionStatus,RO,Trend,Population) %>%
  left_join(TableIncidences,by=c('ADM0NAME'='ctr')) %>%
  left_join(TableCases,by=c('ADM0NAME'='ctr')) %>%
  mutate(CumulativeIncidence=round(TotalCases/Population*100000,1)) %>%
  mutate(RateDeaths=round(TotalDeaths/Population*1000000,1))

return(SummaryTable_AllCountries)

}
