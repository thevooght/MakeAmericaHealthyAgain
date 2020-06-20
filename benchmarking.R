f_prepare_timeseries_data <- function(data) {
  
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
  train_weekly <- train_weekly[-1,] # start from first full week
  
  train_ts_weekly <- ts(data = train_weekly$Freq[train_weekly$Date %in% indTRAIN],
                      start = c(2036,7), end = c(2038,52), frequency = 52)
  # Why start =  c(2036,7)? -> You start 2036 at week 7
  # Why end = c(2038,52)? -> You end in 2038 after 52 weeks
  
  test_ts_weekly <- ts(data = train_weekly$Freq[train_weekly$Date %in% indTEST],
                     start = c(2039,1), end = c(2039,52), frequency = 52)
  # Start at week 1, end in week 52
  
  output <- list(train_weekly, train_ts_weekly, test_ts_weekly)
  return(output)
}

##### Create datasets
datasets <- f_prepare_timeseries_data(data = "basetable")

big_train <- datasets[[1]]
train <- datasets[[2]]
test <- datasets[[3]]

##### Visualize the time series
if(!require('ggfortify')) { install.packages('ggfortify', quietly = TRUE) }; require('ggfortify', quietly = TRUE) #for plotting timeseries
library(ggfortify)  
autoplot(train, color = "blue") + xlab("Weeks") + ylab("Counts")

##### BENCHMARK
if(!require('forecast')) { install.packages('forecast', quietly = TRUE) }; require('forecast', quietly = TRUE)
library(forecast)

### 1) ETS: Exponential Smoothing State Space Model

ets_model <- ets(train, model = "ZZN")
forecast_ets <- forecast(ets_model, h=52) # h = number of periods for forecasting
plot(forecast_ets)


### 2) Arima with external regressors

# Create features data
data_features <- read.csv(file = "basetable.csv",
                          header = TRUE,
                          sep = ',')
str(data_features)
features <- data_features[,-c(1:6)]
features_agg <- aggregate(features, by = list(Date = features$date), mean)
features_agg$Date <- as.Date(features_agg$Date)

# Weekly
features_week <- features_agg %>%
  tq_transmute(select = c(Temperature.F., Humidity..., Pressure.in., Visibility.mi., Wind_Speed.mph., Precipitation.in.),
               mutate_fun = apply.weekly,
               FUN = mean)
features_week$Date <- as.Date(features_week$Date)

# Split in train and test
features_weekTRAIN <- features_week[features_week$Date %in% indTRAIN,]
features_weekTRAIN <- features_weekTRAIN[-c(1),] # was excluded from train, not a full week
features_weekTEST <- features_week[features_week$Date %in% indTEST, ]

# Arima model with external regressors
arima_model_exog  <- auto.arima(train, approximation = FALSE, stepwise = TRUE, xreg = as.matrix(features_weekTRAIN[,-1]))
  # xreg = matrix of external regressors
forecast_arima_exog_week <- forecast(arima_model_exog, h=52, xreg = as.matrix(features_weekTEST[,-1]))
plot(forecast_arima_exog_week)


### 3) NN with external regressors

# Neural net with external regressors
nnetar_model_exog  <- nnetar(train, p = 1, P =1, xreg = as.matrix(features_weekTRAIN[,-1]))
forecast_nnetar_exog_week <- forecast(nnetar_model_exog,h=52,xreg = as.matrix(features_weekTEST[,-1]))
plot(forecast_nnetar_exog_week)

### 4) Ensemble
forecast_ensemble_week <- data.frame(Forecast_ENS = rowMeans(cbind(data.frame(forecast_ets_week)[1], 
                                                                   data.frame(forecast_arima_exog_week)[1], 
                                                                   data.frame(forecast_nnetar_exog_week)[1])))

##### Evaluate performance on out-of-sample set

#In order to calculate the performance of the ensemble, first transform to a TS
forecast_ensemble_week <- ts(forecast_ensemble_week$Forecast_ENS, 
                             start = c(2039,1), end = c(2039,52),
                             frequency = 52)

# RMSE and MAE are scale dependent errors, wheras MAPE and MASE are scale-independent
# SO: use RMSE and MAE to compare monthly and weekly each, MAPE can be used to compare weekly and monthly(!!)
forecast::accuracy(object = forecast_ets_week, x = test)[,c("RMSE","MAE", "MAPE")]
forecast::accuracy(object = forecast_arima_exog_week, x = test)[,c("RMSE","MAE", "MAPE")]
forecast::accuracy(object = forecast_nnetar_exog_week, x = test)[,c("RMSE","MAE", "MAPE")]
forecast::accuracy(object = forecast_ensemble_week, x = test)[,c("RMSE","MAE", "MAPE")]
