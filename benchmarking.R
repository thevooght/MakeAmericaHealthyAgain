f_prepare_benchmark_data <- function(data) {
  
  ### Read in data
  dat <- read.csv(file = paste0(data, ".csv"),
                      header = TRUE,
                      sep = ',')
  
  ### Create dataset
  cols_to_keep <- c("X", "time")
  dat <- dat[ , names(dat) %in% cols_to_keep] # select columns
  
  ### Aggregate accidents per date
  train <- aggregate(dat$X, by=list(Date=dat$date), length)
  names(train)[2] <- 'Freq'
  train$Date <- as.Date(train$Date)
  
  ### Make out-of-period sample => Train set : 2036-2038 / Test set: 2039
  indTRAIN <- as.Date(as.Date("2036-02-08", dateformat = f, origin = "1970-01-01") : as.Date("2038-12-31", origin = "1970-01-01"), dateformat = f, origin = "1970-01-01")
  indTEST <- as.Date(as.Date("2039-01-01", dateformat = f, origin = "1970-01-01") : as.Date("2039-12-31", origin = "1970-01-01"), dateformat = f, origin = "1970-01-01")
  
  ### Create weekly time-series
  if(!require('dplyr')) { install.packages('dplyr', quietly = TRUE) }; require('dplyr', quietly = TRUE)
  library(dplyr)
  
  if(!require('tidyquant')) { install.packages('tidyquant', quietly = TRUE) }; require('tidyquant', quietly = TRUE)
  library(tidyquant)
  
  train_weekly <- train %>% tq_transmute(select = Freq,
                                         mutate_fun = apply.weekly,
                                         FUN = sum)
  train_weekly$Date <- as.Date(train_weekly$Date)
  
  train_ts_weekly <- ts(data = train_weekly$Freq[train_weekly$Date %in% indTRAIN],
                      start = c(2036,7), end = c(2038,52), frequency = 52)
  # Why start =  c(2036,1)? -> You start 2036 at week 7
  # Why end = c(2038,52)? -> You end in 2038 after 52 weeks
  
  test_ts_weekly <- ts(data = train_weekly$Freq[train_weekly$Date %in% indTEST],
                     start = c(2039,1), end = c(2039,52), frequency = 52)
  # Start at week 1, end in week 52
  
  output <- list(train_ts_weekly, test_ts_weekly)
  return(output)
}

datasets <- f_prepare_benchmark_data(data = "basetable")

train <- datasets[[1]]
test <- datasets[[1]]

if(!require('forecast')) { install.packages('forecast', quietly = TRUE) }; require('forecast', quietly = TRUE)
library(forecast)

##### 1) ets: Exponential Smoothing State Space Model

ets_model <- ets(train, model = "ZZN")
forecast_ets_week <- forecast(ets_model, h=52)
plot(forecast_ets_week)



