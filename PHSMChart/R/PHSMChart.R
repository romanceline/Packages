#' PHSMChart
#' This function will build a chart showing the epicurve against PHSM measures
#'
#' @param country the country to build the chart for
#' @param SeverityExcel path to the severity excel file (build by PHSM team)
#'
#' @export
#' @examples
#' @PHSMChart()


PHSMChart<-function(SeverityExcel,country){

  LegendTimeLine<-bind_rows(data.frame(x=seq(0,100,1),Index='Masks',Alpha=seq(0,100,1),Height=3),
                            data.frame(x=seq(0,100,1),Index='Schools',Alpha=seq(0,100,1),Height=3),
                            data.frame(x=seq(0,100,1),Index='Businesses',Alpha=seq(0,100,1),Height=3),
                            data.frame(x=seq(0,100,1),Index='Gatherings',Alpha=seq(0,100,1),Height=3),
                            data.frame(x=seq(0,100,1),Index='Movements',Alpha=seq(0,100,1),Height=3),
                            data.frame(x=seq(0,100,1),Index='Borders',Alpha=seq(0,100,1),Height=3),
                            data.frame(x=seq(0,100,1),Index='GlobalIndex',Alpha=seq(0,100,1),Height=3))

  EpiDataset<-BuildExtendedEpiDataset(country) %>%
    select(ADM0NAME,DateReport,Spline_3DaysAverageCases,Spline_3DaysAverageDeaths)

  StringencyDataset<-SeverityPerCountry(SeverityExcel,country)

  WholeDataset<-merge(EpiDataset,StringencyDataset,by.x=c('DateReport','ADM0NAME'),by.y=c('Date','ADM0NAME'),all=TRUE) %>%
    replace_na(list(Spline_3DaysAverageCases = 0, Spline_3DaysAverageDeaths = 0))
  #This is to make sure both datasets aligned in terms of dates even if we have no data one side or another
  #Important when it comes to align the charts

  StartDate<-min(WholeDataset$DateReport)
  EndDate<-max(StringencyDataset$Date)-1

  EpiCasesToPlot<-WholeDataset %>% select(DateReport,Cases=Spline_3DaysAverageCases) %>% filter(DateReport<=EndDate)
  TopValueCases<-top_n(EpiCasesToPlot,1,Cases)
  maxCases<-round(max(EpiCasesToPlot$Cases),0)
  EpiDeathsToPlot<-WholeDataset %>% select(DateReport,Deaths=Spline_3DaysAverageDeaths) %>% filter(DateReport<=EndDate)
  TopValueDeaths<-top_n(EpiDeathsToPlot,1,Deaths)
  maxDeaths<-round(max(EpiDeathsToPlot$Deaths),0)


  StringencyDataset<-WholeDataset %>% select(DateReport,c(GlobalIndex:Travels)) %>%
    pivot_longer(cols=c(GlobalIndex:Travels)) %>%
    mutate(name=case_when(name=='School'~'Schools',
                          name=='Workplace'~'Businesses',
                          name=='GlobalIndex'~'GlobalIndex',
                          name=='Masks'~'Masks',
                          name=='Gatherings'~'Gatherings',
                          name=='StayHome'~'Movements',
                          name=='Travels'~'Travels')) %>%
    mutate(Height=if_else(name=='GlobalIndex',100,50,50))

  Epicurve_Cases<-ggplot(EpiCasesToPlot)+
    theme_minimal()+
    labs(x="Date of report",y='Number of Cases',linetype=1)+
    geom_area(aes(x=DateReport,y=Cases),color='grey',fill='light grey',size=0.8,show.legend = FALSE)+
    geom_point(data=TopValueCases,aes(x=DateReport,y=Cases))+
    geom_label(data=TopValueCases, aes(x=DateReport,y=Cases,label = round(Cases,0)),hjust=-0.5)+
    scale_x_date(date_breaks = "15 days",date_labels =  "%d-%b",limits=c(StartDate,EndDate))+
    scale_y_continuous(position='left',sec.axis = dup_axis(),limits=c(0,maxCases+1))+
    theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
          axis.text.y.left=element_blank(),axis.title.y.right=element_blank(),axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank())+
    annotate("text", x = StartDate+20, y=0,label = "Number of daily \n cases  over time",vjust=-0.5)+
    theme_void()

  Epicurve_Deaths<-ggplot(EpiDeathsToPlot)+
    theme_minimal()+
    labs(x="Date of report",y='Number of deaths',linetype=1)+
    geom_area(aes(x=DateReport,y=Deaths),color='grey',fill='light grey',size=0.8,show.legend = FALSE)+
    geom_point(data=TopValueDeaths,aes(x=DateReport,y=Deaths))+
    geom_label(data=TopValueDeaths, aes(x=DateReport,y=Deaths,label = round(Deaths,0)),hjust=-0.5)+
    scale_x_date(date_breaks = "15 days",date_labels =  "%d-%b",limits=c(StartDate,EndDate))+
    scale_y_continuous(position='left',sec.axis = dup_axis(),limits=c(0,maxDeaths+1))+
    theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
          axis.text.y.left=element_blank(),axis.title.y.right=element_blank(),axis.title.y.left=element_blank(),
          axis.text.y.right=element_blank())+
    annotate("text", x = StartDate+20, y=0,label = "Number of daily \n deaths over time",vjust=-0.5)+
    theme_void()


  PHSM_TimeLine<-ggplot(StringencyDataset,aes(x=DateReport,y=Height))+
    geom_point(aes(color=value),alpha=0)+
    scale_color_gradient(high = 'black', low = 'white',breaks=c(0,100),labels=c('No measures','Most severe measures'))+
    scale_alpha_continuous(limits = c(0,100),range = c(0, 1))+
    geom_tile(stat='identity',aes(x=DateReport,y=Height,alpha=value,fill=factor(name,levels=c('Masks',"Schools",'Businesses','Gatherings','Movements','Travels',"GlobalIndex"))),position='stack')+
    # annotate("text", x = StartDate+(EndDate-StartDate)*1.9/3, y = 50, label = "PHSM Severity Index",color='white')+
    # annotate("text", x = StartDate+(EndDate-StartDate)*1.9/3, y = 125, label = "International Travel",color='white')+
    # annotate("text", x = StartDate+(EndDate-StartDate)*1.9/3, y = 175, label = "Movements",color='white')+
    # annotate("text", x = StartDate+(EndDate-StartDate)*1.9/3, y = 225, label = "Gatherings",color='white')+
    # annotate("text", x = StartDate+(EndDate-StartDate)*1.9/3, y = 275, label = "Businesses",color='white')+
    # annotate("text", x = StartDate+(EndDate-StartDate)*1.9/3, y = 325, label = "School",color='white')+
    # annotate("text", x = StartDate+(EndDate-StartDate)*1.9/3, y = 375, label = "Masks",color='white')+
    annotate('text', x = EndDate, y=50, label = 'PHSM Severity Index', color='#000000',hjust = 0)+
    annotate("text", x = EndDate, y = 125, label = "International Travel",color='#810E1F',hjust=0)+
    annotate("text", x = EndDate, y = 175, label = "Movements",color='#008DC9',hjust=0)+
    annotate("text", x = EndDate, y = 225, label = "Gatherings",color='#043062',hjust=0)+
    annotate("text", x = EndDate, y = 275, label = "Businesses",color='#0C6E61',hjust=0)+
    annotate("text", x = EndDate, y = 325, label = "Schools",color='#C59507',hjust=0)+
    annotate("text", x = EndDate, y = 375, label = "Masks",color='#D86422',hjust=0)+
    scale_fill_manual(breaks=c('Masks',"Schools",'Businesses','Gatherings','Movements','Travels',"GlobalIndex"),
                      labels=c('Masks','Schools measures', 'Workplace measures','Restrictions on gatherings','Stay-at-home requirements', 'International travel restrictions','Global Severity Index'),
                      values=c('#D86422','#C59507','#0C6E61','#043062','#008DC9','#810E1F','#000000'))+
    scale_x_date(date_breaks = "15 days",date_labels =  "%d-%b",limits=c(StartDate,EndDate))+
    labs(y='Measure Severity',x='',color='PHSM Severity Index Scale')+
    scale_y_continuous(position='left')+
    coord_cartesian(clip = 'off')+
    guides(alpha=FALSE,fill=FALSE,nrow=2)+
    theme_minimal()+
    theme(plot.margin = unit(c(1,5,1,1),"lines"),axis.text.y=element_blank(),axis.text.x = element_text(angle = 90),axis.ticks.y=element_blank(),legend.title=element_text(vjust=0.85),legend.box="vertical",legend.position='bottom',legend.key.width = unit(1.5, "cm"),legend.spacing.y = unit(0.5, 'cm'),legend.spacing.x = unit(1, 'cm'))


  TrickLegend<-ggplot(LegendTimeLine)+
    geom_tile(stat='identity',aes(x=x,y=Height,alpha=Alpha,fill=factor(Index,levels=c('Masks',"Schools",'Businesses','Gatherings','Movements','Borders',"GlobalIndex"))),position='stack')+
    scale_fill_manual(breaks=c('Masks','Schools','Businesses','Gatherings','Movements','Borders','GlobalIndex'),
                      labels=c('Masks','Schools measures', 'Workplace measures', 'Restrictions on gatherings', 'Stay-at-home requirements', 'International travel restrictions','Global Severity Index'),
                      values=c('#D86422','#C59507','#0C6E61','#043062','#008DC9','#810E1F','#000000'))+
    theme_void()+
    theme(legend.position="none",plot.title=element_text(hjust=0.5,size=10))+
    scale_y_continuous(limits=c(-5,NA))+
    scale_x_continuous(limits=c(-100,200))+
    annotate("text", x = -10, y = -3, label = "No measures")+
    annotate("text", x = 110, y = -3, label = "Most severe measures")+
    scale_alpha_continuous(range = c(0, 1))+
    labs(title='Legend PHSM Severity Index Scale')

  ThreePlots<-plot_grid(Epicurve_Cases+theme(legend.position='none'),
                      Epicurve_Deaths+theme(legend.position='none'),
                      PHSM_TimeLine+theme(legend.position='none'),align='v',axis='lr',nrow=3,rel_heights=c(2,2,3))

  Legend<-plot_grid(NA,TrickLegend,NA,ncol=3,rel_widths=c(1,3,1))

  Overview<-plot_grid(ThreePlots,Legend,nrow=2,rel_heights=c(7,1))

  return(Overview)

}



