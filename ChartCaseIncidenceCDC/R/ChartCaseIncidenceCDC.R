#' R chart function
#' This function allows you to create a chart displaying the reproduction number over time
#' @param country - specify the country to draw the chart for
#' @export
#' @examples
#' @ChartCaseIncidenceCDC()

ChartCaseIncidenceCDC <- function(ctr){

  WHO_ref<-WHO_ref %>%
    select(ADM0NAME,UNPOP2019) %>%
    mutate(ADM0NAME=str_to_title(ADM0NAME))

  CountryDataset<-BuildExtendedEpiDataset(ctr) %>%
    mutate(DailyRate=NewCases/UNPOP2019*100000) %>%
    select(ADM0NAME,
           DateReport,
           UNPOP2019,
           TotalCases,
           NewCases,
           DailyRate,
           ThreeDaysAverage_Cases,
           ThreeDaysAverage_Cases_Rate)


  CountryDataset<-CountryDataset %>%
    mutate(CasesLast2Weeks = order_by(DateReport, TotalCases - lag(TotalCases,14)),
           TwoWeekIncidence=CasesLast2Weeks/UNPOP2019*100000)

  CountryDataset<-CountryDataset %>%  filter(!is.na(ThreeDaysAverage_Cases_Rate))


  Spline3DaysAverage_Rate<-smooth.spline(x=CountryDataset$DateReport,
                                    y=CountryDataset$ThreeDaysAverage_Cases_Rate,
                                    spar=0.5)
  ValuesSpline3DaysAverage_Rate<-data.frame(predict(Spline3DaysAverage_Rate,deriv=0),
                                       predict(Spline3DaysAverage_Rate,deriv=1)) %>%
    rename(Spline3DaysAverage_Rate=y,Slope3DaysAverage_Rate=y.1) %>% select(-c(x,x.1))

  CountryDataset <- data.frame(CountryDataset,ValuesSpline3DaysAverage_Rate)

  CountryDataset <-CountryDataset %>%
    group_by(ID = data.table::rleid(Slope3DaysAverage_Rate < 0)) %>%
    mutate(NbDaysSlopeDecreasing = if_else(Slope3DaysAverage_Rate < 0, row_number(), 0L))
  CountryDataset <-CountryDataset %>%
    group_by(ID = data.table::rleid(Slope3DaysAverage_Rate >= 0)) %>%
    mutate(NbDaysSlopeIncreasing = if_else(Slope3DaysAverage_Rate>= 0, row_number(), 0L))

  CountryDataset<-CountryDataset %>%
    mutate(EpidemicStatusCurve=
             case_when((TwoWeekIncidence<=10 & Slope3DaysAverage_Rate>=0.1) ~ "Low Incidence Growth",
                       (TwoWeekIncidence>10 & Slope3DaysAverage_Rate>=0.1) ~ "Elevated Incidence Growth",
                       (TwoWeekIncidence>10 & ((Slope3DaysAverage_Rate>=0 & Slope3DaysAverage_Rate <0.1)|(Slope3DaysAverage_Rate<0 & NbDaysSlopeDecreasing<=5)))~ "Elevated Incidence Plateau",
                       (Slope3DaysAverage_Rate<0 & NbDaysSlopeDecreasing>5) ~ "Sustained Decline",
                       (TwoWeekIncidence<=10 & ((Slope3DaysAverage_Rate>=0 & Slope3DaysAverage_Rate<0.1)|(Slope3DaysAverage_Rate<0 & NbDaysSlopeDecreasing<=5)))~ "Low Incidence Plateau"))

  CountryDataset$EpidemicStatusCurve<-factor(CountryDataset$EpidemicStatusCurve, levels = c("Low Incidence Plateau","Low Incidence Growth","Elevated Incidence Growth","Elevated Incidence Plateau","Sustained Decline"))

  plot<-ggplot(CountryDataset)+
    geom_bar(aes(x=DateReport,y=DailyRate,fill=EpidemicStatusCurve),stat="identity")+
    scale_fill_manual("Epidemic Curve Status",
                      breaks = c("Low Incidence Plateau","Low Incidence Growth","Elevated Incidence Growth","Elevated Incidence Plateau","Sustained Decline"),
                      values=c("#076769","#E47961","#751232","#C65154","#3EA8A6"),drop=FALSE)+
    scale_color_manual(name=NULL,breaks=c('Spline'),labels=c('Spline built on 3-days average \ndaily incidence \n(smoothing parameter = 0.5)'),values=c('black'))+
    scale_x_date(date_labels = "%d-%m",date_breaks = "15 days")+
    geom_line(aes(x=DateReport,y=Spline3DaysAverage_Rate,color="Spline"),linetype=1,size=0.5)+
    theme(legend.title = element_blank(),legend.text = element_text(size=10),legend.key.size=unit(0.2,"cm"),
          plot.title = element_text(size = 10, face = "bold"),axis.title.y = element_text(size=8))+
    labs(x='Date of report',y='Daily Incidence Rate \n (per 100,000 population)')

  return(plot)
}
