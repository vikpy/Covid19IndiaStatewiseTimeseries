library(tidyverse)
library(tseries)
library(forecast)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df <- read.csv("covid19_in_statewise_aggregated.csv")

glimpse(df)

states <-  unique( df$state)


# Total Cases in India 
df_aggr <-  df %>%  group_by(date) %>% summarise(cases_count = sum(cases_count)) 
df_aggr$seq <- seq.int(nrow(df_aggr))
ggplot(data = df_aggr, aes(seq, cases_count)) +
  geom_line(color = "steelblue",size = 1) +
  geom_point(color="steelblue") + 
  labs(title = "Covid19 Cases Count by Days Since First Case",
       subtitle = "(COVID19 Cases since the first case was observed on 30th January 2020)",
       y = "Count of Cases", x = "")

x <-  as.numeric(levels(factor(df$date, levels = c(1:nrow(df_aggr) ) )))
x

x_val <-  rep(x,30)



# First 4 states
ggplot(data = df[df$state %in% states[1:4],], aes(x_val[1:(length(x)*4) ], cases_count)) +
  geom_line(color = "steelblue",size = 1) +
  geom_point(color="steelblue") + 
  labs(title = "Covid19 Cases Count by Days Since First Case StateWise",
       subtitle = "(COVID19 Cases since the first case was observed on 30th January 2020)",
       y = "Count of Cases", x = "") + facet_wrap(~state)

# Next Four Cases 
ggplot(data = df[df$state %in% states[5:8],], aes(x_val[1:(length(x)*4) ] ,cases_count)) +
  geom_line(color = "steelblue",size = 1) +
  geom_point(color="steelblue") + 
  labs(title = "Covid19 Cases Count by Days Since First Case State Wise",
       subtitle = "(COVID19 Cases since the first case was observed on 30th January 2020)",
       y = "Count of Cases", x = "") + facet_wrap(~state)

# Next Four Cases 
ggplot(data = df[df$state %in% states[9:12],], aes(x_val[1:(length(x)*4) ] ,cases_count)) +
  geom_line(color = "steelblue",size = 1) +
  geom_point(color="steelblue") + 
  labs(title = "Covid19 Cases Count by Days Since First Case State Wise",
       subtitle = "(COVID19 Cases since the first case was observed on 30th January 2020)",
       y = "Count of Cases", x = "") + facet_wrap(~state)

# Next Four States

ggplot(data = df[df$state %in% states[13:16],], aes(x_val[1:(length(x)*4) ] ,cases_count)) +
  geom_line(color = "steelblue",size = 1) +
  geom_point(color="steelblue") + 
  labs(title = "Covid19 Cases Count by Days Since First Case State Wise",
       subtitle = "(COVID19 Cases since the first case was observed on 30th January 2020)",
       y = "Count of Cases", x = "") + facet_wrap(~state)



# Next Four States

ggplot(data = df[df$state %in% states[17:20],], aes(x_val[1:(length(x)*4) ] ,cases_count)) +
  geom_line(color = "steelblue",size = 1) +
  geom_point(color="steelblue") + 
  labs(title = "Covid19 Cases Count by Days Since First Case State Wise",
       subtitle = "(COVID19 Cases since the first case was observed on 30th January 2020)",
       y = "Count of Cases", x = "") + facet_wrap(~state)


# Next Four States

ggplot(data = df[df$state %in% states[21:24],], aes(x_val[1:(length(x)*4) ] ,cases_count)) +
  geom_line(color = "steelblue",size = 1) +
  geom_point(color="steelblue") + 
  labs(title = "Covid19 Cases Count by Days Since First Case State Wise",
       subtitle = "(COVID19 Cases since the first case was observed on 30th January 2020)",
       y = "Count of Cases", x = "") + facet_wrap(~state)

# Next Four States

ggplot(data = df[df$state %in% states[25:28],], aes(x_val[1:(length(x)*4) ] ,cases_count)) +
  geom_line(color = "steelblue",size = 1) +
  geom_point(color="steelblue") + 
  labs(title = "Covid19 Cases Count by Days Since First Case State Wise",
       subtitle = "(COVID19 Cases since the first case was observed on 30th January 2020)",
       y = "Count of Cases", x = "") + facet_wrap(~state)

# Next Four States

ggplot(data = df[df$state %in% states[29:30],], aes(x_val[1:(length(x)*2) ] ,cases_count)) +
  geom_line(color = "steelblue",size = 1) +
  geom_point(color="steelblue") + 
  labs(title = "Covid19 Cases Count by Days Since First Case State Wise",
       subtitle = "(COVID19 Cases since the first case was observed on 30th January 2020)",
       y = "Count of Cases", x = "") + facet_wrap(~state)



### Forecast For India Once week
preds_final <- data.frame( state =c(""), pred =  c(0), seq = c(0))

for(state in  states){

  print(state)
  ts <-  as.ts(df[df$state == state,]$cases_count)
  arima.fit <-  auto.arima((ts)^0.5, approximation = FALSE)
  pred_value <-  forecast(arima.fit, h = 8 )
  preds_final <- rbind(preds_final, data.frame( state= state, pred = (pred_value$mean)^2, seq=c(1:8) ) )
} 

preds_final$pred <-  ceiling(preds_final$pred)
preds_final <-  preds_final[2:dim(preds_final)[1], ]

x_1 <-  rep(c(1:8), 30)
ggplot(data = preds_final , aes(x_1,pred)) +
  geom_line(color = "steelblue",size = 1) +
  geom_point(color="steelblue") + 
  labs(title = "Statwise Predictions for Next Week",
       subtitle = "(Next Week Predictions for Indian States for Next Week ie 4th or April to 11th Of April)",
       y = "Count of Cases", x = "") + facet_wrap(~state)


# India Overall 

df_aggr <- preds_final %>%  select(seq,pred) %>%  group_by(seq) %>% summarise(india_preds = sum(pred))

ggplot(data = df_aggr, aes(seq, india_preds)) +
  geom_line(color = "steelblue",size = 1) +
  geom_point(color="steelblue") + 
  labs(title = "Covid19 Cases Count Prediction",
       subtitle = "(Prediction for next Week)",
       y = "Count of Cases", x = "")


### Forecast For India 3 weeks
preds_final <- data.frame( state =c(""), pred =  c(0), seq = c(0))

for(state in  states){
  
  print(state)
  ts <-  as.ts(df[df$state == state,]$cases_count)
  arima.fit <-  auto.arima(ts, approximation = FALSE)
  pred_value <-  forecast(arima.fit, h = 24 )
  preds_final <- rbind(preds_final, data.frame( state= state, pred = pred_value$mean, seq=c(1:24) ) )
} 

preds_final$pred <-  ceiling(preds_final$pred)
preds_final <-  preds_final[2:dim(preds_final)[1], ]

x_1 <-  rep(c(1:24), 30)
ggplot(data = preds_final , aes(x_1,pred)) +
  geom_line(color = "steelblue",size = 1) +
  geom_point(color="steelblue") + 
  labs(title = "Statwise Predictions for Next Week",
       subtitle = "(Next Week Predictions for Indian States for Next Week ie 4th or April to End of April)",
       y = "Count of Cases", x = "") + facet_wrap(~state)


# India Overall 

df_aggr <- preds_final %>%  select(seq,pred) %>%  group_by(seq) %>% summarise(india_preds = sum(pred))

ggplot(data = df_aggr, aes(seq, india_preds)) +
  geom_line(color = "steelblue",size = 1) +
  geom_point(color="steelblue") + 
  labs(title = "Covid19 Cases Count Prediction",
       subtitle = "(Prediction for next 3 Weeks)",
       y = "Count of Cases", x = "")



