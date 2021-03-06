#' TableIndicators()
#' This function will return a table with most basic epi indicators for a specific country
#'
#' @param country the country to build the table for
#'
#' @export
#' @examples
#' @TableIndicators()


TableIndicators<-function(country){

  Dataset_Epiforecast_ <- read.csv("https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/national/cases/summary/rt.csv") %>%
    filter(type=='estimate')

  Rt_country<-function(ctr){
    Dataset_Epiforecast_Country<-WHOCountryNames(Dataset_Epiforecast_,country) %>% filter(country==ctr)
    MaxDate<-max(Dataset_Epiforecast_Country$date)
    DataMaxDate<-Dataset_Epiforecast_Country %>% filter(date==MaxDate)
    return(DataMaxDate)
  }

  Rt_country<-Rt_country(country) %>%
    select(ADM0NAME="country", Last_Update='date',RO='median',CI_low='lower_90', CI_up='upper_90') %>%
    mutate(Trend=if_else(CI_low<1 & CI_up<1,"Decreasing",if_else(CI_low<=1 & CI_up>=1,"Stable","Increasing")))

  Dataset_GlobalPopulation<-WHO_ref %>%
    select(ADM0NAME,UNPOP2019) %>% mutate(ADM0NAME=str_to_title(ADM0NAME)) %>%
    filter(ADM0NAME==country)

  MainDataset<-GetEpiData() %>%
    mutate(DateReport=as.Date(parse_date_time(DateReport, c("dmy", "ymd","mdy"))),ADM0NAME=str_to_title(ADM0NAME)) %>%
    left_join(Dataset_GlobalPopulation,by="ADM0NAME") %>%
    filter(ADM0NAME==country)

  CurrentDate<-max(MainDataset$DateReport)
  FirstDate<-min(MainDataset$DateReport)
  LastReportingDate<-max(MainDataset$DateReport) #(This is meant to change at some point)

  CurrentDataset<-MainDataset %>% filter(DateReport==CurrentDate)

  # NbCases_Days('France',14) would give the number of cases in France over the last 14 days (starting from last reporting date)
  NbCases_Days<-function(Days){
    if (CurrentDate-LastReportingDate<=3){
      Cases<-(((MainDataset %>% filter(DateReport==LastReportingDate))$TotalCases-
                 (MainDataset %>% filter(DateReport==LastReportingDate-Days))$TotalCases))}
    else {
      Cases<-NA
    }
    return(Cases)
  }

  NbDeaths_Days<-function(Days){
    if (CurrentDate-LastReportingDate<=3){
      Cases<-(((MainDataset %>% filter(DateReport==LastReportingDate))$TotalDeaths-
                 (MainDataset %>% filter(DateReport==LastReportingDate-Days))$TotalDeaths))}
    else {
      Cases<-NA
    }
    return(Cases)
  }

  Cases_7Days<-NbCases_Days(7)
  Cases_14Days<-NbCases_Days(14)
  Deaths_7Days<-NbDeaths_Days(7)
  Deaths_14Days<-NbDeaths_Days(14)


  # Function that calculates a certain incidence with a certain delay
  # Ex:
  # - Incidence(14,0) will give the latest 14 days incidence
  # - Incidence(7,7) will give the 7 days incidence one week before last reporting date.
  # The function takes into account potential reporting delays.
  # If the delay is more than 3 days, will return NA.
  Incidence<-function(incidence,delay){
    if (CurrentDate-LastReportingDate<=3){
      Pop<-Dataset_GlobalPopulation$UNPOP2019
      Incidence<-round(((MainDataset %>%  filter(DateReport==LastReportingDate-delay))$TotalCases-
                          (MainDataset %>% filter(DateReport==LastReportingDate-incidence-delay))$TotalCases)/Pop*100000,1)}
    else {
      Incidence<-NA
    }
    return(Incidence)
  }


    FrtDaysIncidence_Latest<-Incidence(14,0)
    FrtDaysIncidence_1WkEarlier<-Incidence(14,7)
    FrtDaysIncidence_2WkEarlier<-Incidence(14,14)
    SvnDaysIncidence_Latest<-Incidence(7,0)
    SvnDaysIncidence_1WkEarlier<-Incidence(7,7)
    SvnDaysIncidence_2WkEarlier<-Incidence(7,14)

    Change14DaysIncidence_2Weeks<-round(FrtDaysIncidence_Latest/FrtDaysIncidence_2WkEarlier*100-100,1)
    Change7DaysIncidence_1week<-round(SvnDaysIncidence_Latest/SvnDaysIncidence_1WkEarlier*100-100,1)

    Table<-data.frame(Indicator=c('Total cases',
                                  'Cases over the last 24h',
                                  'Cases over the last 7 days',
                                  '7-days incidence (per 100.000 population)',
                                  'Change in 7-days incidence (over one week)',
                                  'Cases over the last 14 days',
                                  '14-days incidence (per 100.000 population)',
                                  'Change in 14-days incidence (over 2 weeks)',
                                  'Total deaths',
                                  'Deaths over the last 24h',
                                  'Deaths over the last 7 days',
                                  'Deaths over the last 14 days'),
                      Value=c(round(CurrentDataset$TotalCases,0),
                              round(CurrentDataset$NewCases,0),
                              round(Cases_7Days,0),
                              SvnDaysIncidence_Latest,
                              Change7DaysIncidence_1week,
                              round(Cases_14Days,0),
                              FrtDaysIncidence_Latest,
                              Change7DaysIncidence_1week,
                              round(CurrentDataset$TotalDeaths,0),
                              round(CurrentDataset$NewDeaths,0),
                              round(Deaths_7Days,0),
                              round(Deaths_14Days,0)))

    return(Table)
}



