#' BuildExtendedEpiDataset
#' This function allows you to build an extended dataset of the standard epidataset (with log values, spline values, 3 days average,...)
#' @param Select the country to build the dataset for
#' @export
#' @examples
#' @BuildExtendedEpiDataset()


BuildExtendedEpiDataset<-function(country){
  
  MainDataset<-WHOCountryNames(GetEpiData(),ADM0NAME) %>% 
    mutate(DateReport=as.Date(parse_date_time(DateReport,c("dmy", "ymd","mdy")))) %>% 
    filter(ADM0NAME==country)
  
  MainDataset<-MainDataset %>% 
    filter(!is.na(DateReport)) %>% 
    mutate(NewCases = replace_na(NewCases, 0),
           NewDeaths = replace_na(NewDeaths, 0),
           TotalCases = replace_na(TotalCases, 0),
           TotalDeaths = replace_na(TotalDeaths, 0))
  
  MainDataset<-MainDataset %>% 
    mutate(ThreeDaysAverage_Cases=(NewCases+lag(NewCases,1)+lead(NewCases,1))/3,
           ThreeDaysAverage_Deaths=(NewDeaths+lag(NewDeaths,1)+lead(NewDeaths,1))/3)
  MainDataset<-MainDataset %>% select(-c("WHO_CODE","TotalCases","TotalDeaths","epiWeek")) %>% 
    mutate(log10_MovingAverage_Cases=log10(ThreeDaysAverage_Cases),
           log10_MovingAverage_Deaths=log10(ThreeDaysAverage_Deaths)) %>% 
    mutate(log_cases=log10(NewCases),log_deaths=log10(NewDeaths))
  
  
  MainDataset_Cases<-MainDataset %>% filter(!is.na(ThreeDaysAverage_Cases)) %>% select(DateReport,ThreeDaysAverage_Cases)
  MainDataset_Deaths<-MainDataset %>% filter(!is.na(ThreeDaysAverage_Deaths)) %>% select(DateReport,ThreeDaysAverage_Deaths)
  MainDataset_logCases<-MainDataset %>% filter(!is.na(log10_MovingAverage_Cases)) %>% 
    select(DateReport,log10_MovingAverage_Cases) %>% filter(log10_MovingAverage_Cases!=-Inf)
  MainDataset_logDeaths<-MainDataset %>% filter(!is.na(log10_MovingAverage_Deaths)) %>% 
    select(DateReport,log10_MovingAverage_Deaths) %>% filter(log10_MovingAverage_Deaths!=-Inf)
  Spline_3DaysAverageCases<-smooth.spline(x=MainDataset_Cases$DateReport,y=MainDataset_Cases$ThreeDaysAverage_Cases,spar=0.5)
  Spline_3DaysAverageDeaths<-smooth.spline(x=MainDataset_Deaths$DateReport,y=MainDataset_Deaths$ThreeDaysAverage_Deaths,spar=0.5)
  Spline_3DaysAverage_LogCases<-smooth.spline(x=MainDataset_logCases$DateReport,y=MainDataset_logCases$log10_MovingAverage_Cases,spar=0.5)
  Spline_3DaysAverage_LogDeaths<-smooth.spline(x=MainDataset_logDeaths$DateReport,y=MainDataset_logDeaths$log10_MovingAverage_Deaths,spar=0.5)
  ValuesSpline_3DaysAverageCases<-data.frame(SplineValue_3DaysAverageCases=predict(Spline_3DaysAverageCases,deriv=0))
  ValuesSpline_3DaysAverageDeaths<-data.frame(SplineValue_3DaysAverageDeaths=predict(Spline_3DaysAverageDeaths,deriv=0))
  ValuesSpline_3DaysAverage_LogCases<-data.frame(SplineValue_3DaysAverage_LogCases=predict(Spline_3DaysAverage_LogCases,deriv=0))
  ValuesSpline_3DaysAverage_LogDeaths<-data.frame(SplineValue_3DaysAverage_LogDeaths=predict(Spline_3DaysAverage_LogDeaths,deriv=0))
  MainDataset_Cases<-data.frame(MainDataset_Cases,ValuesSpline_3DaysAverageCases) %>% 
    select(DateReport,"Spline_3DaysAverageCases"="SplineValue_3DaysAverageCases.y")
  MainDataset_Deaths<-data.frame(MainDataset_Deaths,ValuesSpline_3DaysAverageDeaths) %>% 
    select(DateReport,"Spline_3DaysAverageDeaths"="SplineValue_3DaysAverageDeaths.y")
  MainDataset_logCases<-data.frame(MainDataset_logCases,ValuesSpline_3DaysAverage_LogCases) %>% 
    select(DateReport,"Spline_3DaysAverage_logCases"="SplineValue_3DaysAverage_LogCases.y")
  MainDataset_logDeaths<-data.frame(MainDataset_logDeaths,ValuesSpline_3DaysAverage_LogDeaths) %>% 
    select(DateReport,"Spline_3DaysAverage_logDeaths"="SplineValue_3DaysAverage_LogDeaths.y")
  MainDataset_<-MainDataset %>% 
    left_join(MainDataset_Cases,by='DateReport') %>% 
    left_join(MainDataset_Deaths,by='DateReport') %>% 
    left_join(MainDataset_logCases,by='DateReport') %>% 
    left_join(MainDataset_logDeaths,by='DateReport') 
  
  x <- zoo(MainDataset_$Spline_3DaysAverage_logCases,MainDataset_$DateReport)
  x <- na_interpolation(x, option = "linear") %>% fortify.zoo
  
  y <- zoo(MainDataset_$Spline_3DaysAverage_logDeaths,MainDataset_$DateReport)
  y <- na_interpolation(y, option = "linear") %>% fortify.zoo
  
  MainDataset_<-MainDataset_ %>% left_join(x,by=c('DateReport'='Index')) %>% rename(Spline_3DaysAverage_logCases_='.')
  MainDataset_<-MainDataset_ %>% left_join(y,by=c('DateReport'='Index')) %>% rename(Spline_3DaysAverage_logDeaths_='.')
  
  
  return(MainDataset_)
}