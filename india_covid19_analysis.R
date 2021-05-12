
# Use this for RStudio else you will to give the paths 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(rjson)
library(tidyverse)
result <- fromJSON(file = "https://api.covid19india.org/raw_data.json")

str(result)
iter = length(result$raw_data)




patientId <-  c(NA)
reportedOn <- c(NA)
onsetEstimate <- c(NA)
ageEstimate <- c(NA)
gender <-  c(NA)
city <-  c(NA)
district <-  c(NA)
state <- c(NA)
status <-  c(NA)
notes <-  c(NA)
contractedFrom <-  c(NA)
source1 <-  c(NA)
source2 <-  c(NA)
source3 <-  c(NA)
nationality <-  c(NA)
backupnotes <-  c(NA)
statepatientnumber <-  c(NA)
statuschangedate <-  c(NA)
typeoftransmission <- c(NA)

for(iter_value in c(1:iter)){
  
  patientId <- c( patientId , ifelse(is.null(result$raw_data[[iter_value]]$patientnumber) ,NA , result$raw_data[[iter_value]]$patientnumber) )
  
  
  reportedOn <- c( reportedOn , ifelse( is.null(result$raw_data[[iter_value]]$dateannounced), NA,  result$raw_data[[iter_value]]$dateannounced ) )
  
  
  onsetEstimate <- c( onsetEstimate , ifelse( is.null(result$raw_data[[iter_value]]$estimatedonsetdate) ,NA , result$raw_data[[iter_value]]$estimatedonsetdate ) )
  
  
  ageEstimate <- c( ageEstimate , ifelse(is.null(result$raw_data[[iter_value]]$agebracket ),NA ,result$raw_data[[iter_value]]$agebracket ) )
  
  
  gender <- c( gender ,  ifelse( is.null(result$raw_data[[iter_value]]$gender), NA,result$raw_data[[iter_value]]$gender ) )
  
  city <-  c( city ,  ifelse( is.null(result$raw_data[[iter_value]]$detectedcity) , NA, result$raw_data[[iter_value]]$detectedcity)   )
  
  district <-  c( district ,  ifelse( is.null(result$raw_data[[iter_value]]$detecteddistrict),NA , result$raw_data[[iter_value]]$detecteddistrict ) )
  
  state <- c( state ,  ifelse ( is.null(result$raw_data[[iter_value]]$detectedstate), NA,result$raw_data[[iter_value]]$detectedstate ) )
  
  status <-  c( status ,  ifelse( is.null(result$raw_data[[iter_value]]$currentstatus) , NA, result$raw_data[[iter_value]]$currentstatus)   )
  
  notes <- c( notes , ifelse( is.null(result$raw_data[[iter_value]]$notes),NA , result$raw_data[[iter_value]]$notes ) )
  
  contractedFrom <-  c( contractedFrom , ifelse( is.null(result$raw_data[[iter_value]]$contractedfromwhichpatientsuspected), NA , result$raw_data[[iter_value]]$contractedfromwhichpatientsuspected) )
  
  source1 <-  c( source1 ,  ifelse( is.null(result$raw_data[[iter_value]]$source1) , NA , result$raw_data[[iter_value]]$source1) )
  
  source2 <-  c( source2 ,  ifelse( is.null(result$raw_data[[iter_value]]$source2) , NA , result$raw_data[[iter_value]]$source2) ) 
  
  source3 <-  c( source3 ,  ifelse( is.null(result$raw_data[[iter_value]]$source3) , NA , result$raw_data[[iter_value]]$source3) ) 

  nationality <-  c( nationality ,  ifelse( is.null(result$raw_data[[iter_value]]$nationality) , NA , result$raw_data[[iter_value]]$nationality) )     
  
  backupnotes <-  c( backupnotes ,  ifelse( is.null(result$raw_data[[iter_value]]$backupnotes) , NA , result$raw_data[[iter_value]]$backupnotes) )  
  
  statepatientnumber <-  c( statepatientnumber ,  ifelse( is.null(result$raw_data[[iter_value]]$statepatientnumber) , NA , result$raw_data[[iter_value]]$statepatientnumber) ) 

  statuschangedate <-  c( statuschangedate ,  ifelse( is.null(result$raw_data[[iter_value]]$statuschangedate) , NA , result$raw_data[[iter_value]]$statuschangedate) ) 
  
  typeoftransmission <-  c( typeoftransmission ,  ifelse( is.null(result$raw_data[[iter_value]]$typeoftransmission) , NA , result$raw_data[[iter_value]]$typeoftransmission) ) 
  
  
}

covid19_in <-  data.frame(patientId,reportedOn, onsetEstimate, 
                           ageEstimate, gender, 
                          city,
                           district,
                          state,
                           status,
                           notes,
                           contractedFrom,
                           source1,
                           source2,
                           source3,
                          nationality,
                          backupnotes,
                          statepatientnumber,
                          statuschangedate,
                          typeoftransmission
                          )

write.csv(covid19_in[2:dim(covid19_in)[1], ], "covid19_in.csv", row.names=F)

#
# State wise time series 
#
covid19_in <-  covid19_in[2:dim(covid19_in)[1], ]

new_novc <-  covid19_in %>% select(state, patientId,status, reportedOn) %>%  group_by(state, reportedOn,status) %>% summarise(cases_count = n()) 



new_novc_conc <-  new_novc %>% mutate(key = paste( state, "_" , reportedOn)  ) %>%  select(key, cases_count, status) 
new_novc_conc_df <-  data.frame(key = new_novc_conc$key, value= new_novc_conc$cases_count, status = new_novc_conc$status)
new_novc_conc_df
novc_spread <-  spread(new_novc_conc_df, key= status, value = value)
novc_spread <-  separate(novc_spread, col = c("key") , into = c("state", "date"), sep=c("_") ) 
novc_spread[is.na(novc_spread)] <- 0
novc_spread <-  novc_spread %>%  mutate( date = str_squish(date)) 
novc_spread$date <-  as.Date(novc_spread$date,"%d/%m/%y")  
test <- novc_spread %>%  group_by(state, date) %>% summarise_all(funs(sum))
nCov19_final <-  as.data.frame(novc_spread) 
nCov19_final_1 <-  nCov19_final %>%  mutate(key = paste( state, "_" , date)  ) %>%  select(key, Deceased, Hospitalized, Migrated, Recovered)
nCov19_final_2 <-  gather(nCov19_final_1, "status", "cases",2:5) %>%  separate(col = c("key") , into = c("state", "date"), sep=c("_") ) %>% mutate(key = paste( state, "_" , status)  ) %>%  select( key, date, cases ) %>% spread(key = date, value = cases)
nCov19_final_2[is.na(nCov19_final_2)] <- 0
#nCov19_final$date <-  as.Date(nCov19_final$date, "%y-%m-%d")


# Taking Care of the missing dates
min_date <-  as.character(as.Date(min(nCov19_final$date[!is.na(nCov19_final$date) ]), "%y-%m-%d"))
max_date <-  as.character( as.Date(max(nCov19_final$date[!is.na(nCov19_final$date) ]), "%y-%m-%d"))
date_generation <-  as.character(seq.POSIXt(as.POSIXct(min_date), as.POSIXct(max_date), by="day"))
present_dates <-  str_squish(colnames(nCov19_final_2)[2:length(colnames(nCov19_final_2))])
missing_date <-  date_generation[!(date_generation %in% present_dates)]
nCov19_final_2[missing_date] <- 0

# Gathering the Dates   
no_of_dates  <-  length(colnames(nCov19_final_2))

nCov19_final_3 <-  nCov19_final_2 %>% gather("date", "cases",2:no_of_dates) %>% mutate(date = as.Date(str_squish(date)) )  %>%  arrange(key, date)  %>%  group_by(key) %>%  mutate(cases = cumsum(cases)) %>%  separate(col = c("key") , into = c("state", "status"), sep=c("_") )
# Removing zeros in the date column 
nCov19_final_3 <- nCov19_final_3[!is.na(nCov19_final_3$date) ,] 


write.csv(nCov19_final_3, "covid19_in_statewise_timeseries.csv", row.names=F) 

nCov19_final_3 <- nCov19_final_3 %>%  select(state, date, cases) %>%  group_by(state, date ) %>% summarise(cases_count = sum(cases))
nCov19_final_3 <- as.data.frame(nCov19_final_3) %>% mutate( state = str_squish(state))
nCov19_final_3 <-  drop_na(nCov19_final_3[nCov19_final_3$state != "", ])
write.csv(nCov19_final_3, "covid19_in_statewise_aggregated.csv",row.names=F) 



# Extracting from a different source 

results <-  fromJSON(file="https://api.covid19india.org/states_daily.json")


iter <- length(results$states_daily)

series <- data.frame(state = c(""), status = c(NA), date = c(NA), cases = c(NA))

values <- names(as.vector(results$states_daily[[100]]))
values <- values[!(values %in% c("status", "total", "date"))]
for( iter_value in c(1:iter)){
  print(paste("As on: ",as.vector(results$states_daily[[iter_value]])["date"][[1]]) )
  for( val in values){
    series <-  rbind( series , data.frame(state=val, status = as.vector(results$states_daily[[iter_value]])["status"][[1]] , date = as.vector(results$states_daily[[iter_value]])["date"][[1]] , cases = as.vector(results$states_daily[[iter_value]])[val][[1]]  ) )
    
  }
}

series <- series[2:length(series),]
series_1 <- series %>%  arrange(state, status)  %>%  group_by(state, status) %>%  mutate(cases = cumsum(cases))
series_1
write.csv(nCov19_final_3, "covid19_in_statewise_timeseries_new.csv") 






# City Wise


covid19_in <-  covid19_in[2:dim(covid19_in)[1], ]

covid19_in <-  covid19_in[covid19_in$district  == "Mumbai",]


new_novc <-  covid19_in %>% select(district, patientId,status, reportedOn) %>%  group_by(district, reportedOn,status) %>% summarise(cases_count = n()) 



new_novc_conc <-  new_novc %>% mutate(key = paste( district, "_" , reportedOn)  ) %>%  select(key, cases_count, status) 
new_novc_conc_df <-  data.frame(key = new_novc_conc$key, value= new_novc_conc$cases_count, status = new_novc_conc$status)
new_novc_conc_df
novc_spread <-  spread(new_novc_conc_df, key= status, value = value)
novc_spread <-  separate(novc_spread, col = c("key") , into = c("district", "date"), sep=c("_") ) 
novc_spread[is.na(novc_spread)] <- 0
novc_spread <-  novc_spread %>%  mutate( date = str_squish(date)) 
novc_spread$date <-  as.Date(novc_spread$date,"%d/%m/%y")  
test <- novc_spread %>%  group_by(district, date) %>% summarise_all(funs(sum))
nCov19_final <-  as.data.frame(novc_spread) 



nCov19_final_1 <-  nCov19_final %>%  mutate(key = paste( district, "_" , date)  ) %>%  select(key, Deceased, Hospitalized)
nCov19_final_2 <-  gather(nCov19_final_1, "status", "cases",2:3) %>%  separate(col = c("key") , into = c("district", "date"), sep=c("_") ) %>% mutate(key = paste( district, "_" , status)  ) %>%  select( key, date, cases ) %>% spread(key = date, value = cases)
nCov19_final_2[is.na(nCov19_final_2)] <- 0
#nCov19_final$date <-  as.Date(nCov19_final$date, "%y-%m-%d")


# Taking Care of the missing dates
min_date <-  as.character(as.Date(min(nCov19_final$date[!is.na(nCov19_final$date) ]), "%y-%m-%d"))
max_date <-  as.character( as.Date(max(nCov19_final$date[!is.na(nCov19_final$date) ]), "%y-%m-%d"))
date_generation <-  as.character(seq.POSIXt(as.POSIXct(min_date), as.POSIXct(max_date), by="day"))
present_dates <-  str_squish(colnames(nCov19_final_2)[2:length(colnames(nCov19_final_2))])
missing_date <-  date_generation[!(date_generation %in% present_dates)]
nCov19_final_2[missing_date] <- 0

# Gathering the Dates   
no_of_dates  <-  length(colnames(nCov19_final_2))

nCov19_final_3 <-  nCov19_final_2 %>% gather("date", "cases",2:no_of_dates) %>% mutate(date = as.Date(str_squish(date)) )  %>%  arrange(key, date)  %>%  group_by(key) %>%  mutate(cases = cumsum(cases)) %>%  separate(col = c("key") , into = c("district", "status"), sep=c("_") )
# Removing zeros in the date column 
nCov19_final_3 <- nCov19_final_3[!is.na(nCov19_final_3$date) ,] 


write.csv(nCov19_final_3, "covid19_in_statewise_timeseries_1.csv", row.names=F) 

nCov19_final_3 <- nCov19_final_3 %>%  select(district, date, cases) %>%  group_by(district, date ) %>% summarise(cases_count = sum(cases))
nCov19_final_3 <- as.data.frame(nCov19_final_3) %>% mutate( district = str_squish(district))
nCov19_final_3 <-  drop_na(nCov19_final_3[nCov19_final_3$district != "", ])
write.csv(nCov19_final_3, "covid19_in_MUMBAI_aggregated.csv",row.names=F) 





