#' WHO_Names function
#' This function will transform any country name as per WHO convention
#' @param dataset - dataset containing country names to convert to WHO reference
#' @param countryfield - name of the field containing country names - Don't use quotes!
#' @export

WHOCountryNames <- function(dataset,countryfield) {

  who_names<-WHO_ref %>% select(ADM0NAME) %>%
    mutate(ADM0NAME=str_to_title(ADM0NAME))

  countryfield <- enquo(countryfield)
  NewDataset<-dataset %>%
    mutate(!!countryfield:=str_to_title(!!countryfield)) %>%
    mutate(!!countryfield:=case_when(!!countryfield %in% who_names$ADM0NAME ~ !!countryfield,
                                     !!countryfield=='Czechia' ~ 'Czech Republic',
                                     !!countryfield=='Moldova' ~ 'Republic Of Moldova',
                                     !!countryfield=='Russia' ~ 'Russian Federation',
                                     !!countryfield=='Russian Fed.' ~ 'Russian Federation',
                                     !!countryfield=='UK' ~ 'United Kingdom'))

  return(NewDataset)
}
