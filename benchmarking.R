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
  
  # WEEKLY
  dat_agg_weekly <- dat_agg %>% tq_transmute(select = Freq,
                                             mutate_fun = apply.weekly,
                                             FUN = sum)
  dat_agg_weekly$Date <- as.Date(dat_agg_weekly$Date)
  dat_agg_weekly <- dat_agg_weekly[-1,] # start from first full week
  
  # Create weekly time-series & split in training and test set
  train_ts_weekly <- ts(data = dat_agg_weekly$Freq[dat_agg_weekly$Date %in% indTRAIN],
                        start = c(2036,7), end = c(2038,52), frequency = 52)
  # Why start = c(2036,7)? -> You start 2036 at week 7 (after removing the first uncomplete week)
  # Why end = c(2038,52)? -> You end in week 52 in 2038
  
  test_ts_weekly <- ts(data = dat_agg_weekly$Freq[dat_agg_weekly$Date %in% indTEST],
                       start = c(2039,1), end = c(2039,52), frequency = 52)
  # Start at week 1, end in week 52
  
  # MONTHLY
  dat_agg_monthly <- dat_agg %>% tq_transmute(select = Freq,
                                              mutate_fun = apply.monthly,
                                              FUN = sum)
  dat_agg_monthly$Date <- as.Date(dat_agg_monthly$Date)
  dat_agg_monthly <- dat_agg_monthly[-1,] # start from first full month
  
  # Create weekly time-series & split in training and test set
  train_ts_montly <- ts(data = dat_agg_monthly$Freq[dat_agg_monthly$Date %in% indTRAIN],
                        start = c(2036,3), end = c(2038,12), frequency = 12)
  # Why start = c(2036,3)? -> You start 2036 at month 3 (= March)
  # Why end = c(2038,12)? -> You end in month 12 of 2038
  
  test_ts_monthly <- ts(data = dat_agg_monthly$Freq[dat_agg_monthly$Date %in% indTEST],
                       start = c(2039,1), end = c(2039,12), frequency = 12)
  # Start at month 1, end in month 12
  
  output <- list(train_ts_weekly, test_ts_weekly, train_ts_montly, test_ts_monthly)
  return(output)
}

##### Create datasets
datasets <- f_prepare_timeseries(data = "basetable")
train_weeks <- datasets[[1]]
test_weeks <- datasets[[2]]
train_months <- datasets[[3]]
test_months <- datasets[[4]]

##### Visualize the time series
if(!require('ggfortify')) { install.packages('ggfortify', quietly = TRUE) }; require('ggfortify', quietly = TRUE) #for plotting timeseries
library(ggfortify)
# Weekly
autoplot(train_weeks, color = "blue") + xlab("Weeks") + ylab("Counts")
# Monthly
autoplot(train_months, color = "blue") + xlab("Months") + ylab("Counts")

##### BENCHMARK
if(!require('forecast')) { install.packages('forecast', quietly = TRUE) }; require('forecast', quietly = TRUE)
library(forecast)

### 1) ETS: Exponential Smoothing State Space Model

# WEEKLY
ets_week <- ets(train_weeks, model = "ZZN")
fc_ets_week <- forecast(ets_week, h=52) # h = number of periods for forecasting
plot(fc_ets_week) # ANN = simple exponential smoothing

# MONTHLY
ets_month <- ets(train_months, model = "ZZN")
fc_ets_month <- forecast(ets_month, h=12) # h = number of periods for forecasting
plot(fc_ets_month)

### 2) Arima model

# WEEKLY
arima_week  <- auto.arima(train_weeks, approximation=FALSE, stepwise=FALSE)
fc_arima_week <- forecast(arima_week, h=52)
plot(fc_arima_week)

# MONTHLY
arima_month  <- auto.arima(train_months, approximation=FALSE, stepwise=FALSE)
fc_arima_month <- forecast(arima_month, h=12)
plot(fc_arima_month)

### 3) Neural Net

# WEEKLY
nnetar_week  <- nnetar(train_weeks, p=1, P=1)
fc_nnetar_week <- forecast(nnetar_week, h=52)
plot(fc_nnetar_week)

# MONTHLY
nnetar_month  <- nnetar(train_months, p=1, P=1)
fc_nnetar_month <- forecast(nnetar_month, h=12)
plot(fc_nnetar_month)

### 4) Ensemble

# WEEKLY
fc_ensemble_week <- data.frame(fc_ENS = rowMeans(cbind(data.frame(fc_ets_week)[1], 
                                                       data.frame(fc_arima_week)[1], 
                                                       data.frame(fc_nnetar_week)[1])))
# MONTHLY
#issue in forecast of nnetar -> transform output
fc_nnetar_month_transformed <- as.data.frame(t(data.frame(fc_nnetar_month)))
names(fc_nnetar_month_transformed)[1] <- 'Point.Forecast'
fc_nnetar_month_transformed$Point.Forecast <- as.double(fc_nnetar_month_transformed$Point.Forecast)
#compute ensemble
fc_ensemble_month <- data.frame(fc_ENS = rowMeans(cbind(data.frame(fc_ets_month)[1], 
                                                        data.frame(fc_arima_month)[1], 
                                                        data.frame(fc_nnetar_month_transformed)[1])))

### Evaluate performance on out-of-sample set

# In order to calculate the performance of the ensemble, first transform to a TS
fc_ensemble_week <- ts(fc_ensemble_week$fc_ENS, 
                       start = c(2039,1), end = c(2039,52),
                       frequency = 52)
fc_ensemble_month <- ts(fc_ensemble_month$fc_ENS, 
                        start = c(2039,1), end = c(2039,12),
                        frequency = 12)

# RMSE and MAE are scale dependent errors, wheras MAPE and MASE are scale-independent
# SO: use RMSE and MAE to compare monthly and weekly each, MAPE can be used to compare weekly vs. monthly

# WEEKLY
forecast::accuracy(object = fc_ets_week, x = test_weeks)[,c("RMSE","MAE", "MAPE")]
forecast::accuracy(object = fc_arima_week, x = test_weeks)[,c("RMSE","MAE", "MAPE")]
forecast::accuracy(object = fc_nnetar_week, x = test_weeks)[,c("RMSE","MAE", "MAPE")]
forecast::accuracy(object = fc_ensemble_week, x = test_weeks)[,c("RMSE","MAE", "MAPE")]

# MONTHLY
forecast::accuracy(object = fc_ets_month, x = test_months)[,c("RMSE","MAE", "MAPE")]
forecast::accuracy(object = fc_arima_month, x = test_months)[,c("RMSE","MAE", "MAPE")]
forecast::accuracy(object = fc_nnetar_month, x = test_months)[,c("RMSE","MAE", "MAPE")]
forecast::accuracy(object = fc_ensemble_month, x = test_months)[,c("RMSE","MAE", "MAPE")]
