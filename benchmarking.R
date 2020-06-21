f_prepare_timeseries <- function(data) {
  
  ### Read in data
  dat <- read.csv(file = paste0(data, ".csv"),
                      header = TRUE,
                      sep = ',')
  
  ### Create dataset
  cols_to_keep <- c("X", "date")
  dat <- dat[ , names(dat) %in% cols_to_keep] # select columns
  
  ### Aggregate accidents per date
  dat_agg <- aggregate(dat$X, by=list(Date=dat$date), length)
  names(dat_agg)[2] <- 'Freq'
  dat_agg$Date <- as.Date(dat_agg$Date)
  
  ### Make out-of-period sample => Train set : 2036-2038 / Test set: 2039
  indTRAIN <- as.Date(as.Date("2036-02-08", dateformat = f, origin = "1970-01-01") : as.Date("2038-12-31", origin = "1970-01-01"), dateformat = f, origin = "1970-01-01")
  indTEST <- as.Date(as.Date("2039-01-01", dateformat = f, origin = "1970-01-01") : as.Date("2039-12-31", origin = "1970-01-01"), dateformat = f, origin = "1970-01-01")
  
  ### Compute number of accidents per week
  if(!require('dplyr')) { install.packages('dplyr', quietly = TRUE) }; require('dplyr', quietly = TRUE)
  library(dplyr)
  
  if(!require('tidyquant')) { install.packages('tidyquant', quietly = TRUE) }; require('tidyquant', quietly = TRUE)
  library(tidyquant)
  
  dat_agg_weekly <- dat_agg %>% tq_transmute(select = Freq,
                                             mutate_fun = apply.weekly,
                                             FUN = sum)
  dat_agg_weekly$Date <- as.Date(dat_agg_weekly$Date)
  dat_agg_weekly <- dat_agg_weekly[-1,] # start from first full week
  
  ### Create weekly time-series & split in training and test set
  train_ts_weekly <- ts(data = dat_agg_weekly$Freq[dat_agg_weekly$Date %in% indTRAIN],
                        start = c(2036,6), end = c(2038,52), frequency = 52)
  # Why start =  c(2036,7)? -> You start 2036 at week 7 (after removing the first uncomplete week)
  # Why end = c(2038,52)? -> You end in 2038 after 52 weeks
  
  test_ts_weekly <- ts(data = dat_agg_weekly$Freq[dat_agg_weekly$Date %in% indTEST],
                       start = c(2039,1), end = c(2039,52), frequency = 52)
  # Start at week 1, end in week 52
  
  output <- list(dat_agg_weekly, train_ts_weekly, test_ts_weekly)
  return(output)
}

f_create_features <- function(data){
  
  ### Create features data
  data_features <- read.csv(file = paste0(data, ".csv"),
                            header = TRUE,
                            sep = ',')
  #str(data_features)
  cols_to_keep <- c("Temperature.F.", "Humidity...", "Pressure.in.", "Visibility.mi.", "Wind_Speed.mph.", "Precipitation.in.", "date")
  features <- data_features[,cols_to_keep]
  
  # Set "time" as date -> writing to csv has made this chr again (Wind direction and weather condition too)
  f <- "%Y-%M-%d"
  data_features$date <- as.Date(as.character(data_features$date), dateformat = f)
  
  ### Make out-of-period sample => Train set : 2036-2038 / Test set: 2039
  indTRAIN <- as.Date(as.Date("2036-02-08", dateformat = f, origin = "1970-01-01") : as.Date("2038-12-31", origin = "1970-01-01"), dateformat = f, origin = "1970-01-01")
  indTEST <- as.Date(as.Date("2039-01-01", dateformat = f, origin = "1970-01-01") : as.Date("2039-12-31", origin = "1970-01-01"), dateformat = f, origin = "1970-01-01")
  
  # Create train and test set
  train_features <- data_features[data_features$date %in% indTRAIN,]
  test_features <- data_features[data_features$date %in% indTEST, ]
  
  # NA's in wind_speed, visibility, temp, humidity, pressure -> impute
  if(!require('imputeMissings')) { install.packages('imputeMissings', quietly = TRUE) }; require('imputeMissings', quietly = TRUE)
  library(imputeMissings)
  cols_to_impute <- c("Temperature.F.", "Humidity...", "Pressure.in.", "Visibility.mi.", "Wind_Speed.mph.")
  train_features[cols_to_impute] <- imputeMissings::impute(train_features[cols_to_impute])
  test_features[cols_to_impute] <- imputeMissings::impute(test_features[cols_to_impute])
  # numeric/integer vectors are imputed with the median
  
  # Define feature columns (without date variable)
  #cols_features <- c("Temperature.F.", "Humidity...", "Pressure.in.", "Visibility.mi.", "Wind_Speed.mph.", "Precipitation.in.")
  
  # Prepare training set
  train_features_agg <- aggregate(train_features, by = list(Date = train_features$date), mean)
  train_features_agg$Date <- as.Date(train_features_agg$Date)
  
  train_features_week <- train_features_agg %>%
    tq_transmute(select = c(Temperature.F., Humidity..., Pressure.in., Visibility.mi., Wind_Speed.mph., Precipitation.in.),
                 mutate_fun = apply.weekly,
                 FUN = mean)
  train_features_week$Date <- as.Date(train_features_week$Date)
  train_features_week <- train_features_week[-1,] # start from first full week
  
  # Prepare test set
  test_features_agg <- aggregate(test_features, by = list(Date = test_features$date), mean)
  test_features_agg$Date <- as.Date(test_features_agg$Date)
  
  test_features_week <- test_features_agg %>%
    tq_transmute(select = c(Temperature.F., Humidity..., Pressure.in., Visibility.mi., Wind_Speed.mph., Precipitation.in.),
                 mutate_fun = apply.weekly,
                 FUN = mean)
  test_features_week$Date <- as.Date(test_features_week$Date)
  test_features_week <- test_features_week[-1,] # start from first full week
  
  # Add a logical for whether there was a holiday that week
  holidays <- read.csv(file = 'Public-Holiday.csv',
                       header = TRUE,
                       sep = ';')
  # Fix date column
  names(holidays)[1] <- 'Date'
  holidays$Date <- as.Date(as.character(holidays$Date), format = '%d/%m/%Y')
  # Function to get the next sunday
  nextweekday <- function(date, weekday) {   
    date <- as.Date(date)
    diff <- weekday - wday(date)
    diff = if_else(diff < 0, diff+7, diff)
    return(date + diff)
  }
  holidays$Date <- nextweekday(date = holidays$Date, weekday = 1) # weekday = 1 stands for sunday
  train_features_week$HasHoliday <- ifelse(train_features_week$Date %in% holidays$Date, 1, 0)
  test_features_week$HasHoliday <- ifelse(test_features_week$Date %in% holidays$Date, 1, 0)

  output <- list(train_features_week, test_features_week)
  return(output)
}

##### Create datasets
datasets <- f_prepare_timeseries(data = "basetable")

big_train <- datasets[[1]]
train <- datasets[[2]]
test <- datasets[[3]]

##### Visualize the time series
if(!require('ggfortify')) { install.packages('ggfortify', quietly = TRUE) }; require('ggfortify', quietly = TRUE) #for plotting timeseries
library(ggfortify)  
autoplot(big_train, color = "blue") + xlab("Weeks") + ylab("Counts")

##### BENCHMARK
if(!require('forecast')) { install.packages('forecast', quietly = TRUE) }; require('forecast', quietly = TRUE)
library(forecast)


### 1) ETS: Exponential Smoothing State Space Model

ets_model <- ets(train, model = "ZZN")
forecast_ets_week <- forecast(ets_model, h=52) # h = number of periods for forecasting
plot(forecast_ets_week)


### 2) Arima with external regressors

# Create external regressors
features <- f_create_features(data = "basetable")
features_train <- features[[1]]
features_test <- features[[2]]

# Arima model with external regressors
arima_model_exog  <- auto.arima(train, approximation = FALSE, stepwise = TRUE, xreg = as.matrix(features_train[,-1])) # exclude date column
  # xreg = matrix of external regressors
forecast_arima_exog_week <- forecast(arima_model_exog, h=52, xreg = as.matrix(features_test[,-1]))
plot(forecast_arima_exog_week)


### 3) NN with external regressors

# Neural net with external regressors
nnetar_model_exog  <- nnetar(train, p = 1, P =1, xreg = as.matrix(features_train[,-1]))
forecast_nnetar_exog_week <- forecast(nnetar_model_exog, h=52, xreg = as.matrix(features_test[,-1]))
plot(forecast_nnetar_exog_week)


### 4) Ensemble
forecast_ensemble_week <- data.frame(Forecast_ENS = rowMeans(cbind(data.frame(forecast_ets_week)[1], 
                                                                   data.frame(forecast_arima_exog_week)[1], 
                                                                   data.frame(forecast_nnetar_exog_week)[1])))

### Evaluate performance on out-of-sample set

# In order to calculate the performance of the ensemble, first transform to a TS
forecast_ensemble_week <- ts(forecast_ensemble_week$Forecast_ENS, 
                             start = c(2039,1), end = c(2039,52),
                             frequency = 52)

# RMSE and MAE are scale dependent errors, wheras MAPE and MASE are scale-independent
# SO: use RMSE and MAE to compare monthly and weekly each, MAPE can be used to compare weekly and monthly(!!)
forecast::accuracy(object = forecast_ets_week, x = test)[,c("RMSE","MAE", "MAPE")]
forecast::accuracy(object = forecast_arima_exog_week, x = test)[,c("RMSE","MAE", "MAPE")]
forecast::accuracy(object = forecast_nnetar_exog_week, x = test)[,c("RMSE","MAE", "MAPE")]
forecast::accuracy(object = forecast_ensemble_week, x = test)[,c("RMSE","MAE", "MAPE")]
