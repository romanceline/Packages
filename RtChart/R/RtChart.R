#' R chart function
#' This function allows you to create a chart displaying the reproduction number over time
#' @param country - specify the country to draw the chart for


RtChart <- function(ctr){

  require(dplyr)

  Dataset_Rt <- read.csv("https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/national/cases/summary/rt.csv")
  Dataset_Rt <- WHO_EURO_countryname(Dataset_Rt,country)

  Dataset_Rt_Country<-Dataset_Rt %>% filter(country==ctr) %>%
    filter(type!='forecast') %>% select(country,date,R=median,lower_90,upper_90) %>%
    mutate(date=as.Date(parse_date_time(date, c("dmy", "ymd","mdy")))) %>%
    group_by(ID = data.table::rleid(R < 1)) %>%
    mutate(NbDays = if_else(R < 1, row_number(), 0L)) %>%
    mutate(Classification=if_else(R<=1 & NbDays>=14,"R is lower than 1 since more than 2 weeks",
                              if_else(R<=1,"R is lower than 1","R is higher than 1")))
    Dataset_Rt_Country$Classification<-factor(Dataset_Rt_Country$Classification, levels = c("R is higher than 1","R is lower than 1","R is lower than 1 since more than 2 weeks"))

    Dataset_Rt_Country<-Dataset_Rt_Country %>% filter(date<=Sys.Date())

    ForPolygon_Up<-Dataset_Rt_Country %>% select(date,coord=upper_90)
    ForPolygon_Low<-Dataset_Rt_Country %>% select(date,coord=lower_90) %>% arrange(desc(date))
    ForPolygon<-bind_rows(ForPolygon_Up,ForPolygon_Low)

    minDate<-min(Dataset_Rt_Country$date)
    maxDate<-max(Dataset_Rt_Country$date)

    chart<-ggplot()+
      geom_polygon(data=ForPolygon,aes(x=date,y=coord,fill='Confidence Interval 90'),alpha=0.3)+
      geom_line(data=Dataset_Rt_Country,aes(x=date,y=R,color=Classification,group=1),size=0.73)+
      scale_x_date(date_labels = "%d-%m-%y",date_breaks = "15 days",limits=c(minDate,maxDate))+
      labs(y="Rt",x='Date of report')+
      theme(legend.title = element_blank(),legend.text = element_text(size=10),legend.key.size=unit(0.2,"cm"),
            plot.title = element_text(size = 10, face = "bold"),axis.title.y = element_text(size=10))+
      scale_color_manual(values=c("#C65154","#BBE4D1","#3EA8A6"),
                         breaks=c("R is higher than 1","R is lower than 1","R is lower than 1 since more than 2 weeks"),
                         labels=c("Rt higher than 1","Rt lower than 1","Rt lower than 1 \nfor more than 2 weeks"),drop=FALSE)+
      scale_fill_manual(values=c("#494850"),
                        breaks=c("Confidence Interval 90"),drop=FALSE)+
      geom_hline(aes(yintercept=1),linetype="dashed")
    #plot_grid(chart+theme(legend.position="none"),get_legend(p2),ncol=2,rel_widths=c(8,2),align="vh")
  return(chart)
}
