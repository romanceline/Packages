#' GetEpiData function
#' This function read the running epi data online
#' @export
#' 


GetEpiData <- function(){
  get_data<-function(server,service_name,layer_id=0,w="1=1",rg="false",of="*"){
  #gets data from an Esri feature service.
  #
  # args:
  #   server (string): name of the arcgis server
  #   service_name (string): name of the feature service
  #   layer id (int): id of the layer, default = 0
  #   w (string): where condition, default = "1=1",
  #   rg(string): defines whether geometry is exported. Must be either 'false' or 'true'. Note values must be a string not a boolean
  #   of (string): list of output fields. Must be a string not a list or vector. Default is set to export all columns ("*)
  #   f (string): output format
  #
  # returns:
  # data (tibble): feature service converted to a tibble
  
  out <- tryCatch(
    {
      # Get the maximum id. This is need to define the number of calls required to fetch data
      res <- content(GET(url = paste(server,service_name,"FeatureServer",layer_id,"query?",sep="/"),
                         query = list(
                           where = w,
                           returnGeometry = rg,
                           returnIdsOnly = TRUE,
                           f = "json"),
                         as = "text", encoding = "UTF-8")) %>%
        fromJSON(., flatten = TRUE)
      
      n_records <- res %>% .[2] %>%  as.data.frame(.) %>%#get second list and convert into a data frame
        summarise(max_id=max(.[1]), count=n())
      
      id_field <- unlist(res[1])
      
      
      # Do until all records are exported
      # Retrieve recrords from the feature service and merge into a single data frame
      id <- 0
      data <- tibble()
      while (id<n_records$max_id) {
        
        records <- content(GET(url = paste(server,service_name,"FeatureServer",layer_id,"query?",sep="/"),
                               query = list(
                                 where = paste(w,"and",id_field,">=",id) ,
                                 returnGeometry = rg,
                                 outFields = of,
                                 returnIdsOnly = FALSE,
                                 f = "json")),
                           as = "text", encoding = "UTF-8") %>%
          fromJSON(., flatten = TRUE)  %>% .$features %>% as_tibble() %>%
          rename_all(list(~ sub(".*?[[:punct:]]", "",.)))
        
        id_new <- records %>% summarise(max(!!as.name(id_field))) %>% pull
        if(nrow(records)==0 | id_new == id ) {
          #if no records are returned, break loop
          break
        } else{
          data <- rbind(data,records)
          id <- id_new +1
          
        }
      }
      
      #convert esri data to dttm
      data <- data %>%  mutate_if(~ (is.double(.) && min(., na.rm = TRUE) > 1e12 && min(., na.rm = TRUE) %% 1000 ==0) , list(~as.POSIXct(./1000,origin="1970-01-01",tz="GMT")))
      
      # retrun data
      return(data)
    },
    error = function(cond) {
      message(paste0("Request failed. Please review the paramaters."))
      #message(request$status_code)
      message(cond)
      
    })
}


server <- "https://services.arcgis.com/5T5nSi527N4F7luB/ArcGIS/rest/services"
service_daily <- "EURO_COVID19_Running_v3"

MainDataset<-get_data(server, service_daily)

return(MainDataset)}