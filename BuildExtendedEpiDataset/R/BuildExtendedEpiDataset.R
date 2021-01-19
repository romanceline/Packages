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

  WHO_ref<-WHOCountryNames(WHO_ref,ADM0NAME) %>% select(ADM0NAME,UNPOP2019)

  MainDataset<-MainDataset %>% left_join(WHO_ref,by='ADM0NAME')

  MainDataset<-MainDataset %>%
    filter(!is.na(DateReport)) %>%
    mutate(NewCases = replace_na(NewCases, 0),
           NewDeaths = replace_na(NewDeaths, 0),
           TotalCases = replace_na(TotalCases, 0),
           TotalDeaths = replace_na(TotalDeaths, 0),
           NewCases_Rate=NewCases/UNPOP2019*100000,
           NewDeaths_Rate=NewDeaths/UNPOP2019*1000000)

  MainDataset<-MainDataset %>%
    mutate(ThreeDaysAverage_Cases=(NewCases+lag(NewCases,1)+lead(NewCases,1))/3,
           ThreeDaysAverage_Deaths=(NewDeaths+lag(NewDeaths,1)+lead(NewDeaths,1))/3) %>%
    mutate(ThreeDaysAverage_Cases_Rate=(NewCases_Rate+lag(NewCases_Rate,1)+lead(NewCases_Rate,1))/3,
           ThreeDaysAverage_Deaths_Rate=(NewDeaths_Rate+lag(NewDeaths_Rate,1)+lead(NewDeaths_Rate,1))/3)
  MainDataset<-MainDataset %>% select(-c("WHO_CODE","epiWeek"))
  # %>% mutate(log10_MovingAverage_Cases=log10(ThreeDaysAverage_Cases),
  #          log10_MovingAverage_Deaths=log10(ThreeDaysAverage_Deaths),
  #          log10_MovingAverage_Cases_Rate=log10(ThreeDaysAverage_Cases_Rate),
  #          log10_MovingAverage_Deaths_Rate=log10(ThreeDaysAverage_Deaths_Rate)) %>%
  #   mutate(log_cases=log10(NewCases),log_deaths=log10(NewDeaths),
  #          log_cases_rate=log10(NewCases_Rate),log_deaths_rate=log10(NewDeaths_Rate))


  MainDataset_Cases<-MainDataset %>%
    filter(!is.na(ThreeDaysAverage_Cases)) %>%
    select(DateReport,ThreeDaysAverage_Cases)
  MainDataset_Cases_Rate<-MainDataset %>%
    filter(!is.na(ThreeDaysAverage_Cases_Rate)) %>%
    select(DateReport,ThreeDaysAverage_Cases_Rate)
  MainDataset_Deaths<-MainDataset %>%
    filter(!is.na(ThreeDaysAverage_Deaths)) %>%
    select(DateReport,ThreeDaysAverage_Deaths)
  MainDataset_Deaths_Rate<-MainDataset %>%
    filter(!is.na(ThreeDaysAverage_Deaths_Rate)) %>%
    select(DateReport,ThreeDaysAverage_Deaths_Rate)
  # MainDataset_logCases<-MainDataset %>%
  #   filter(!is.na(log10_MovingAverage_Cases)) %>%
  #   select(DateReport,log10_MovingAverage_Cases) %>%
  #   filter(log10_MovingAverage_Cases!=-Inf)
  # MainDataset_logCases_Rate<-MainDataset %>%
  #   filter(!is.na(log10_MovingAverage_Cases_Rate)) %>%
  #   select(DateReport,log10_MovingAverage_Cases_Rate) %>%
  #   filter(log10_MovingAverage_Cases_Rate!=-Inf)
  # MainDataset_logDeaths<-MainDataset %>%
  #   filter(!is.na(log10_MovingAverage_Deaths)) %>%
  #   select(DateReport,log10_MovingAverage_Deaths) %>%
  #   filter(log10_MovingAverage_Deaths!=-Inf)
  # MainDataset_logDeaths_Rate<-MainDataset %>%
  #   filter(!is.na(log10_MovingAverage_Deaths_Rate)) %>%
  #   select(DateReport,log10_MovingAverage_Deaths_Rate) %>%
  #   filter(log10_MovingAverage_Deaths_Rate!=-Inf)
  Spline_3DaysAverageCases<-smooth.spline(x=MainDataset_Cases$DateReport,y=MainDataset_Cases$ThreeDaysAverage_Cases,spar=0.5)
  Spline_3DaysAverageDeaths<-smooth.spline(x=MainDataset_Deaths$DateReport,y=MainDataset_Deaths$ThreeDaysAverage_Deaths,spar=0.5)
  # Spline_3DaysAverage_LogCases<-smooth.spline(x=MainDataset_logCases$DateReport,y=MainDataset_logCases$log10_MovingAverage_Cases,spar=0.5)
  # Spline_3DaysAverage_LogDeaths<-smooth.spline(x=MainDataset_logDeaths$DateReport,y=MainDataset_logDeaths$log10_MovingAverage_Deaths,spar=0.5)

  Spline_3DaysAverageCases_Rate<-smooth.spline(x=MainDataset_Cases_Rate$DateReport,y=MainDataset_Cases_Rate$ThreeDaysAverage_Cases_Rate,spar=0.5)
  Spline_3DaysAverageDeaths_Rate<-smooth.spline(x=MainDataset_Deaths_Rate$DateReport,y=MainDataset_Deaths_Rate$ThreeDaysAverage_Deaths_Rate,spar=0.5)
  # Spline_3DaysAverage_LogCases_Rate<-smooth.spline(x=MainDataset_logCases_Rate$DateReport,y=MainDataset_logCases_Rate$log10_MovingAverage_Cases_Rate,spar=0.5)
  # Spline_3DaysAverage_LogDeaths_Rate<-smooth.spline(x=MainDataset_logDeaths_Rate$DateReport,y=MainDataset_logDeaths_Rate$log10_MovingAverage_Deaths_Rate,spar=0.5)

  ValuesSpline_3DaysAverageCases<-data.frame(SplineValue_3DaysAverageCases=predict(Spline_3DaysAverageCases,deriv=0))
  ValuesSpline_3DaysAverageDeaths<-data.frame(SplineValue_3DaysAverageDeaths=predict(Spline_3DaysAverageDeaths,deriv=0))
  # ValuesSpline_3DaysAverage_LogCases<-data.frame(SplineValue_3DaysAverage_LogCases=predict(Spline_3DaysAverage_LogCases,deriv=0))
  # ValuesSpline_3DaysAverage_LogDeaths<-data.frame(SplineValue_3DaysAverage_LogDeaths=predict(Spline_3DaysAverage_LogDeaths,deriv=0))

  ValuesSpline_3DaysAverageCases_Rate<-data.frame(SplineValue_3DaysAverageCases_Rate=predict(Spline_3DaysAverageCases_Rate,deriv=0))
  ValuesSpline_3DaysAverageDeaths_Rate<-data.frame(SplineValue_3DaysAverageDeaths_Rate=predict(Spline_3DaysAverageDeaths_Rate,deriv=0))
  # ValuesSpline_3DaysAverage_LogCases_Rate<-data.frame(SplineValue_3DaysAverage_LogCases_Rate=predict(Spline_3DaysAverage_LogCases_Rate,deriv=0))
  # ValuesSpline_3DaysAverage_LogDeaths_Rate<-data.frame(SplineValue_3DaysAverage_LogDeaths_Rate=predict(Spline_3DaysAverage_LogDeaths_Rate,deriv=0))


  MainDataset_Cases<-data.frame(MainDataset_Cases,ValuesSpline_3DaysAverageCases) %>%
    select(DateReport,"Spline_3DaysAverageCases"="SplineValue_3DaysAverageCases.y")
  MainDataset_Deaths<-data.frame(MainDataset_Deaths,ValuesSpline_3DaysAverageDeaths) %>%
    select(DateReport,"Spline_3DaysAverageDeaths"="SplineValue_3DaysAverageDeaths.y")
  # MainDataset_logCases<-data.frame(MainDataset_logCases,ValuesSpline_3DaysAverage_LogCases) %>%
  #   select(DateReport,"Spline_3DaysAverage_logCases"="SplineValue_3DaysAverage_LogCases.y")
  # MainDataset_logDeaths<-data.frame(MainDataset_logDeaths,ValuesSpline_3DaysAverage_LogDeaths) %>%
  #   select(DateReport,"Spline_3DaysAverage_logDeaths"="SplineValue_3DaysAverage_LogDeaths.y")

  MainDataset_Cases_Rate<-data.frame(MainDataset_Cases_Rate,ValuesSpline_3DaysAverageCases_Rate) %>%
    select(DateReport,"Spline_3DaysAverageCases_Rate"="SplineValue_3DaysAverageCases_Rate.y")
  MainDataset_Deaths_Rate<-data.frame(MainDataset_Deaths_Rate,ValuesSpline_3DaysAverageDeaths_Rate) %>%
    select(DateReport,"Spline_3DaysAverageDeaths_Rate"="SplineValue_3DaysAverageDeaths_Rate.y")
  # MainDataset_logCases_Rate<-data.frame(MainDataset_logCases_Rate,ValuesSpline_3DaysAverage_LogCases_Rate) %>%
  #   select(DateReport,"Spline_3DaysAverage_logCases_Rate"="SplineValue_3DaysAverage_LogCases_Rate.y")
  # MainDataset_logDeaths_Rate<-data.frame(MainDataset_logDeaths_Rate,ValuesSpline_3DaysAverage_LogDeaths_Rate) %>%
  #   select(DateReport,"Spline_3DaysAverage_logDeaths_Rate"="SplineValue_3DaysAverage_LogDeaths_Rate.y")

  MainDataset_<-MainDataset %>%
    left_join(MainDataset_Cases,by='DateReport') %>%
    left_join(MainDataset_Deaths,by='DateReport') %>%
    # left_join(MainDataset_logCases,by='DateReport') %>%
    # left_join(MainDataset_logDeaths,by='DateReport') %>%
    left_join(MainDataset_Cases_Rate,by='DateReport') %>%
    left_join(MainDataset_Deaths_Rate,by='DateReport') %>%
    # left_join(MainDataset_logCases_Rate,by='DateReport') %>%
    # left_join(MainDataset_logDeaths_Rate,by='DateReport')

  # x <- zoo(MainDataset_$Spline_3DaysAverage_logCases,MainDataset_$DateReport)
  # x <- na_interpolation(x, option = "linear") %>% fortify.zoo
  #
  # y <- zoo(MainDataset_$Spline_3DaysAverage_logDeaths,MainDataset_$DateReport)
  # y <- na_interpolation(y, option = "linear") %>% fortify.zoo
  #
  # z <- zoo(MainDataset_$Spline_3DaysAverage_logCases_Rate,MainDataset_$DateReport)
  # z <- na_interpolation(z, option = "linear") %>% fortify.zoo
  #
  # a <- zoo(MainDataset_$Spline_3DaysAverage_logDeaths_Rate,MainDataset_$DateReport)
  # a <- na_interpolation(a, option = "linear") %>% fortify.zoo

  # MainDataset_<-MainDataset_ %>%
  #   left_join(x,by=c('DateReport'='Index')) %>%
  #   rename(Spline_3DaysAverage_logCases_='.')
  # MainDataset_<-MainDataset_ %>%
  #   left_join(y,by=c('DateReport'='Index')) %>%
  #   rename(Spline_3DaysAverage_logDeaths_='.')
  # MainDataset_<-MainDataset_ %>%
  #   left_join(z,by=c('DateReport'='Index')) %>%
  #   rename(Spline_3DaysAverage_logCases_Rate_='.')
  # MainDataset_<-MainDataset_ %>%
  #   left_join(a,by=c('DateReport'='Index')) %>%
  #   rename(Spline_3DaysAverage_logDeaths_Rate_='.')

  return(MainDataset_)
}
