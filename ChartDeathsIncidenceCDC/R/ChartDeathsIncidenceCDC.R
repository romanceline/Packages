#' R chart function
#' This function allows you to create a chart displaying the reproduction number over time
#' @param country - specify the country to draw the chart for
#' @export
#' @examples
#' @ChartDeathsIncidenceCDC()


ChartDeathsIncidenceCDC<-function(country){
  
  CountryDataset<-BuildExtendedEpiDataset(country) %>% 
    select(ADM0NAME,DateReport,NewDeaths,NewDeaths_Rate,UNPOP2019,ThreeDaysAverage_Deaths_Rate)
  
  CountryDataset <- CountryDataset %>% drop_na()
  
  minDate<-min(CountryDataset$DateReport)
  maxDate<-max(CountryDataset$DateReport)
  
  Spline3DaysAverage<-smooth.spline(x=CountryDataset$DateReport,y=CountryDataset$ThreeDaysAverage_Deaths_Rate,spar=0.5)
  ValuesSpline3DaysAverage<-data.frame(predict(Spline3DaysAverage,deriv=0),predict(Spline3DaysAverage,deriv=1)) %>% 
    rename(Spline3DaysAverage=y,Slope3DaysAverage=y.1) %>% select(-c(x,x.1))
  
  CountryDataset <- data.frame(CountryDataset,ValuesSpline3DaysAverage)
  
  CountryDataset<-CountryDataset %>% 
    mutate(SlopeClassification=if_else(Slope3DaysAverage<=0,"Decline","Increase")) 
  
  
  CountryDataset <-CountryDataset %>% 
    group_by(ID = data.table::rleid(SlopeClassification == 'Decline')) %>%
    mutate(DaysDecline = if_else(SlopeClassification == 'Decline', row_number(), 0L))
  
  CountryDataset<- CountryDataset %>% 
    mutate(Indicator=if_else(DaysDecline==0,"",
                             if_else(DaysDecline>=21,
                                     "Daily rate of deaths is declining since more than 3 weeks",
                                     "Daily rate of deaths is declining")))
  
  CountryDataset$Indicator<-factor(CountryDataset$Indicator, 
                                   levels = c("","Daily rate of deaths is declining",
                                              "Daily rate of deaths is declining since more than 3 weeks"))
  
  plot<-ggplot(CountryDataset)+
    geom_bar(aes(x=DateReport,y=NewDeaths_Rate,fill=Indicator),stat="identity")+
    geom_line(aes(x=DateReport,y=Spline3DaysAverage,color='Spline'))+
    scale_fill_manual(name=NULL,values=c("#C65154","#BBE4D1","#3EA8A6"),
                      breaks=c("","Daily rate of deaths is declining","Daily rate of deaths is declining since more than 3 weeks"),
                      labels=c("Increase","Continuous decline","Continuous decline for \nmore than 3 weeks"),drop=FALSE)+
    scale_color_manual(name=NULL,breaks=c('Spline'),labels=c('Spline built on 3-days average \ndaily deaths rate \n(smoothing parameter = 0.5)'),values=c('black'))+
    scale_x_date(date_labels = "%d-%m",date_breaks = "30 days",limits=c(minDate,maxDate))+
    labs(y="Daily Deaths Rate \n (per 1.000.000 population)",x='Date of report')+
    guides(colour = guide_legend(order = 2),
           fill = guide_legend(order = 1))+
    theme(legend.title = element_blank(),legend.text = element_text(size=10),legend.key.size=unit(0.2,"cm"),
          plot.title = element_text(size = 10, face = "bold"),axis.title.y = element_text(size=8))
  plot<-plot_grid(plot+theme(legend.position="none"),get_legend(plot),ncol=2,rel_widths=c(8,2),align="vh")
  
  return(plot)
}