##########################################################################################################
#                                      MAKE AMERICA HEALTHY AGAIN                                        #
##########################################################################################################
# 2. BENCHMARKING                                                                                        #
##########################################################################################################
# Group 17                                                                                               #
# Regis Demolie, Cedric Devooght, Nathan De Wilde, Florian Mathieu, Jef Van Hulle                        #
##########################################################################################################

rm(list = ls())
#dir <- 'C:/! Project Prescriptive'
dir <- paste0(getwd(), "/data")
setwd(dir = dir)
getwd()

### FUNCTIONS
######################################################################################################

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

f_create_features <- function(data){
  
  ### Create features data
  data_features <- read.csv(file = paste0(data, ".csv"),
                            header = TRUE,
                            sep = ',')
  #str(data_features)
  cols_to_keep <- c("Temperature.F.", "Humidity...", "Pressure.in.", "Visibility.mi.", "Wind_Speed.mph.", "Precipitation.in.", "date")
  data_features <- data_features[,cols_to_keep]
  
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
  
  ## Prepare training set
  train_features_agg <- aggregate(train_features, by = list(Date = train_features$date), mean)
  train_features_agg$Date <- as.Date(train_features_agg$Date)
  #week
  train_features_week <- train_features_agg %>%
    tq_transmute(select = c(Temperature.F., Humidity..., Pressure.in., Visibility.mi., Wind_Speed.mph., Precipitation.in.),
                 mutate_fun = apply.weekly,
                 FUN = mean)
  train_features_week$Date <- as.Date(train_features_week$Date)
  train_features_week <- train_features_week[2:(nrow(train_features_week)-1),]
  # start from first full week & remove last week as this is also an incomplete week (and is not considered in the TS)
  #month
  train_features_month <- train_features_agg %>%
    tq_transmute(select = c(Temperature.F., Humidity..., Pressure.in., Visibility.mi., Wind_Speed.mph., Precipitation.in.),
                 mutate_fun = apply.monthly,
                 FUN = mean)
  train_features_month$Date <- as.Date(train_features_month$Date)
  train_features_month <- train_features_month[2:(nrow(train_features_month)),] #start from first full month
  
  ## Prepare test set
  #week
  test_features_agg <- aggregate(test_features, by = list(Date = test_features$date), mean)
  test_features_agg$Date <- as.Date(test_features_agg$Date)
  
  test_features_week <- test_features_agg %>%
    tq_transmute(select = c(Temperature.F., Humidity..., Pressure.in., Visibility.mi., Wind_Speed.mph., Precipitation.in.),
                 mutate_fun = apply.weekly,
                 FUN = mean)
  test_features_week$Date <- as.Date(test_features_week$Date)
  test_features_week <- test_features_week[-1,] # start from first full week
  #month
  test_features_agg <- aggregate(test_features, by = list(Date = test_features$date), mean)
  test_features_agg$Date <- as.Date(test_features_agg$Date)
  
  test_features_month <- test_features_agg %>%
    tq_transmute(select = c(Temperature.F., Humidity..., Pressure.in., Visibility.mi., Wind_Speed.mph., Precipitation.in.),
                 mutate_fun = apply.monthly,
                 FUN = mean)
  test_features_month$Date <- as.Date(test_features_month$Date)
  
  output <- list(train_features_week, test_features_week, train_features_month,test_features_month)
  return(output)
}

##### BENCHMARKING
######################################################################################################

##### Create datasets
datasets <- f_prepare_timeseries(data = "basetable")
train_weeks <- datasets[[1]]
test_weeks <- datasets[[2]]
train_months <- datasets[[3]]
test_months <- datasets[[4]]

##### Create features
datasets <- f_create_features(data = "basetable")
train_fweek <- datasets[[1]]
test_fweek<- datasets[[2]]
train_fmonth <- datasets[[3]]
test_fmonth<- datasets[[4]]

##### Visualize the time series
if(!require('ggfortify')) { install.packages('ggfortify', quietly = TRUE) }; require('ggfortify', quietly = TRUE) #for plotting timeseries
library(ggfortify)
autoplot(train_weeks, color = "blue") + xlab("Weeks") + ylab("Counts") # Weekly
autoplot(train_months, color = "blue") + xlab("Months") + ylab("Counts") # Monthly

##### BENCHMARK
if(!require('forecast')) { install.packages('forecast', quietly = TRUE) }; require('forecast', quietly = TRUE)
library(forecast)

### 1) Naive: Any forecasting method should be evaluated by being compared to a naive method
fc_naive_week = snaive(train_weeks, h=52)
plot(fc_naive_week)

fc_naive_month = snaive(train_months, h=12)
plot(fc_naive_month)

### 2) ETS: Exponential Smoothing State Space Model
ets_week <- ets(train_weeks, model = "ZZN")
fc_ets_week <- forecast(ets_week, h=52) # h = number of periods for forecasting
plot(fc_ets_week) # ANN = simple exponential smoothing

ets_month <- ets(train_months, model = "ZZN")
fc_ets_month <- forecast(ets_month, h=12)
plot(fc_ets_month)

### 3) Arima model
#w/o features
arima_week  <- auto.arima(train_weeks, approximation=FALSE, stepwise=FALSE)
fc_arima_week <- forecast(arima_week, h=52)
plot(fc_arima_week)

arima_month  <- auto.arima(train_months, approximation=FALSE, stepwise=FALSE)
fc_arima_month <- forecast(arima_month, h=12)
plot(fc_arima_month)

#w features
arima_exog_week  <- auto.arima(train_weeks, approximation = FALSE, stepwise = TRUE, xreg = as.matrix(train_fweek[,-1]))
fc_arima_exog_week <- forecast(arima_exog_week, h=52, xreg = as.matrix(test_fweek[,-1]))
plot(fc_arima_exog_week)

arima_exog_month <- auto.arima(train_months, approximation = FALSE, stepwise = TRUE, xreg = as.matrix(train_fmonth[,-1])) 
fc_arima_exog_month <- forecast(arima_exog_month, h=52, xreg = as.matrix(test_fmonth[,-1]))
plot(fc_arima_exog_month)

### 4) Neural Net
#w/o features
nnetar_week  <- nnetar(train_weeks)
fc_nnetar_week <- forecast(nnetar_week, h=52)
plot(fc_nnetar_week)

nnetar_month  <- nnetar(train_months)
fc_nnetar_month <- forecast(nnetar_month, h=12)
plot(fc_nnetar_month)

#w features
nnetar_exog_week  <- nnetar(train_weeks, p = 1, P =1, xreg = as.matrix(train_fweek[,-1]))
fc_nnetar_exog_week <- forecast(nnetar_exog_week, h=52, xreg = as.matrix(test_fweek[,-1]))
plot(fc_nnetar_exog_week)

nnetar_exog_month  <- nnetar(train_months, p = 1, P =1, xreg = as.matrix(train_fmonth[,-1]))
fc_nnetar_exog_month <- forecast(nnetar_exog_month, h=52, xreg = as.matrix(test_fmonth[,-1]))
plot(fc_nnetar_exog_month)

### 5) STL: Seasonal and Trend decomposition using Loess
stl_week <- stlf(train_weeks)
fc_stl_week <- forecast(stl_week, h=52)  
plot(fc_stl_week)

stl_month <- stlf(train_months)
fc_stl_month <- forecast(stl_month, h=12)  
plot(fc_stl_month)

### 6) Random Walk
stl_rw_week <- stlf(train_weeks, forecastfunction=rwf) # apply random walk
fc_stl_rw_week <- forecast(stl_rw_week, h=52) 
plot(fc_stl_rw_week)

stl_rw_month <- stlf(train_months, forecastfunction=rwf) # apply random walk
fc_stl_rw_month <- forecast(stl_rw_month, h=12)  
plot(fc_stl_rw_month)

### 7) TBATS
# BATS and TBATS allow for multiple seasonalities. 
# TBATS is a modification (an improvement really) of BATS that allows for multiple non-integer seasonality cycles.
tbats_week = tbats(train_weeks)
fc_tbats_week = forecast(tbats_week, h=52)
plot(fc_tbats_week)

tbats_month = tbats(train_months)
fc_tbats_month = forecast(tbats_month, h=12)
plot(fc_tbats_month)

### 7) Ensemble
fc_ensemble_week <- data.frame(fc_ENS = rowMeans(cbind(data.frame(fc_ets_week)[1],
                                                       data.frame(fc_arima_week)[1], 
                                                       data.frame(fc_nnetar_week)[1],
                                                       data.frame(fc_stl_week)[1],
                                                       data.frame(fc_stl_rw_week)[1],
                                                       data.frame(fc_tbats_week)[1])))
# Transform monthly output of neural net
fc_nnetar_month_transformed <- as.data.frame(t(data.frame(fc_nnetar_month)))
names(fc_nnetar_month_transformed)[1] <- 'Point.Forecast'
fc_nnetar_month_transformed$Point.Forecast <- as.numeric(as.character(fc_nnetar_month_transformed$Point.Forecast))
# Compute ensemble
fc_ensemble_month <- data.frame(fc_ENS = rowMeans(cbind(data.frame(fc_ets_month)[1],
                                                        data.frame(fc_arima_month)[1], 
                                                        data.frame(fc_nnetar_month_transformed)[1],
                                                        data.frame(fc_stl_month)[1],
                                                        data.frame(fc_stl_rw_month)[1],
                                                        data.frame(fc_tbats_month)[1])))

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

# Weekly
naive_week_acc <- forecast::accuracy(object = fc_naive_week, x = test_weeks)[,c("RMSE", "MAE", "MAPE")]
ets_week_acc <- forecast::accuracy(object = fc_ets_week, x = test_weeks)[,c("RMSE", "MAE", "MAPE")]
arima_week_acc <- forecast::accuracy(object = fc_arima_week, x = test_weeks)[,c("RMSE", "MAE", "MAPE")]
arima_exog_week_acc <- forecast::accuracy(object = fc_arima_exog_week, x = test_months)[,c("RMSE", "MAE", "MAPE")]
nnetar_week_acc <- forecast::accuracy(object = fc_nnetar_week, x = test_weeks)[,c("RMSE", "MAE", "MAPE")]
nnetar_exog_week_acc <- forecast::accuracy(object = fc_nnetar_exog_week, x = test_months)[,c("RMSE", "MAE", "MAPE")]
stl_week_acc <- forecast::accuracy(object = fc_stl_week, x = test_weeks)[,c("RMSE", "MAE", "MAPE")]
stl_rw_week_acc <- forecast::accuracy(object = fc_stl_rw_week, x = test_weeks)[,c("RMSE", "MAE", "MAPE")]
tbats_week_acc <- forecast::accuracy(object = fc_tbats_week, x = test_weeks)[,c("RMSE", "MAE", "MAPE")]
ensemble_week_acc <- forecast::accuracy(object = fc_ensemble_week, x = test_weeks)[,c("RMSE", "MAE", "MAPE")]

# Monthly
naive_month_acc <- forecast::accuracy(object = fc_naive_month, x = test_months)[,c("RMSE", "MAE", "MAPE")]
ets_month_acc <- forecast::accuracy(object = fc_ets_month, x = test_months)[,c("RMSE", "MAE", "MAPE")]
arima_month_acc <- forecast::accuracy(object = fc_arima_month, x = test_months)[,c("RMSE", "MAE", "MAPE")]
arima_exog_month_acc <- forecast::accuracy(object = fc_arima_exog_month, x = test_months)[,c("RMSE", "MAE", "MAPE")]
nnetar_month_acc <- forecast::accuracy(object = fc_nnetar_month, x = test_months)[,c("RMSE", "MAE", "MAPE")]
nnetar_exog_month_acc <- forecast::accuracy(object = fc_nnetar_exog_month, x = test_months)[,c("RMSE", "MAE", "MAPE")]
stl_month_acc <- forecast::accuracy(object = fc_stl_month, x = test_months)[,c("RMSE", "MAE", "MAPE")]
stl_rw_month_acc <- forecast::accuracy(object = fc_stl_rw_month, x = test_months)[,c("RMSE", "MAE", "MAPE")]
tbats_month_acc <- forecast::accuracy(object = fc_tbats_month, x = test_months)[,c("RMSE", "MAE", "MAPE")]
ensemble_month_acc <- forecast::accuracy(object = fc_ensemble_month, x = test_months)[,c("RMSE", "MAE", "MAPE")]

print(naive_week_acc)
print(ets_week_acc)
print(arima_week_acc)
print(arima_exog_week_acc)
print(nnetar_week_acc)
print(nnetar_exog_week_acc)
print(stl_week_acc)
print(stl_rw_week_acc)
print(tbats_week_acc)
print(ensemble_week_acc)

print(naive_month_acc)
print(ets_month_acc)
print(arima_month_acc)
print(arima_exog_month_acc)
print(nnetar_month_acc)
print(nnetar_exog_month_acc)
print(stl_month_acc)
print(stl_rw_month_acc)
print(tbats_month_acc)
print(ensemble_month_acc)

### Conclusion
# Naive & tbats has a much higher training error than test error => counter intuitive and therefore not trustworthy
# We will exclude them from further consideration
# (We probably should have more data)
# When looking at the MAPE, we notice monthly data performs better
## 1) each  technique has a higher MAPE when performed on weekly data, compared to the same technique on monthly data 
## 2) overall, the 4 lowest MAPE's are achieved by techniques on monthly data

#When including RMSE and MAE for monthly data, we notice two good candidates: the ensemble, and neural net w/o features
#neural net: lower RMSE and MAE
#ensemble: lower MAPE
#We've chosen to continue with the neural net, as this lowers the runtime of our forecast.
##### Producing graphs for slides

#Compare the training and test error for Naive and TBATS
graph <- rbind(naive_week_acc, naive_month_acc,tbats_month_acc,tbats_week_acc)
#naive and tbats
barplot(naive_month_acc[,-3], beside = TRUE,  col=c('dark blue', 'white'),legend.text = TRUE, main='Naive Bayes for monthly data')
barplot(naive_week_acc[,-3], beside = TRUE, col=c('dark blue', 'white'), main='Naive Bayes for weekly data ')
barplot(naive_month_acc[,3], beside = TRUE,  col=c('dark blue', 'white'),space = 0, main='Naive Bayes MAPE for monthly data')
barplot(naive_week_acc[,3], beside = TRUE, col=c('dark blue', 'white'), space = 0, main='Naive Bayes MAPE for weekly data')
barplot(tbats_month_acc[,3], beside = TRUE, col=c('dark blue', 'white'), space = 0,main='TBATS MAPE for monthly data')
barplot(tbats_week_acc[,3], beside = TRUE, col=c('dark blue', 'white'), space= 0, main='TBATS MAPE for weekly data')

#MAPE to compare weekly and monhly data
graph <- data.frame( 'MAPE' = rbind(ets_week_acc[2,3], ets_month_acc[2,3], arima_week_acc[2,3], arima_month_acc[2,3],
                                    arima_exog_week_acc[2,3] ,arima_exog_month_acc[2,3], nnetar_week_acc[2,3],nnetar_month_acc[2,3], 
                                    nnetar_exog_week_acc[2,3] ,nnetar_exog_month_acc[2,3], stl_week_acc[2,3], stl_month_acc[2,3], 
                                    stl_rw_week_acc[2,3], stl_rw_month_acc[2,3], ensemble_week_acc[3], ensemble_month_acc[3]),
                     'name' = c('weekly ETS', 'monthly ETS', 'weekly ARIMA 
                                w/o features', 'monthly ARIMA 
                                w/o features','weekly ARIMA 
                                with features', 'monthly ARIMA
                                with features', 'weekly neural net
                                w/o features','monthly neural net
                                w/o features', 'weekly neural net
                                with features','monthly neural net
                                with features', 'weekly STL', 'monthly STL', 
                                'weekly STL & random walk', 'monthly STL & random walk',
                                'weekly ensemble', 'monthly ensemble'))
graph$Value <- ifelse(grepl('weekly',graph$name,fixed = TRUE),'Weekly', 'Monthly') 
library("ggplot2")
library('tidyverse')
library(forcats)
p <- graph %>%
  mutate(name = fct_reorder(name,-MAPE)) %>%
  ggplot(aes(x= name, y = MAPE ))+
  geom_bar(stat = 'identity',aes(fill = Value, group = Value))+ 
  labs(title = 'MAPE', y='',x='')+ 
  scale_fill_manual("Legend", values = c("Monthly" = "navyblue", "Weekly" = "forestgreen"))+
  theme(panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = "transparent"), 
        legend.box.background = element_rect(fill = "transparent"),
        axis.line.y  = element_line(colour = "black"),
        axis.ticks = element_blank(),
        text = element_text(size=10))+
  coord_flip()
ggsave(p, filename = "MAPE.png",  bg = "transparent")
#RMSE monthly
graph <- data.frame( 'RMSE' = rbind(ets_month_acc[2,1], arima_month_acc[2,1],
                                    arima_exog_month_acc[2,1], nnetar_exog_month_acc[2,1],
                                    nnetar_month_acc[2,1], stl_month_acc[2,1],
                                    stl_rw_month_acc[2,1], ensemble_month_acc[1]),
                     'name' = c('ETS', 'ARIMA without features','ARIMA with features',
                                'neural net 
                                with features' ,'neural net 
                                without features',
                                'STL', 'STL & random walk',
                                'ensemble'))

p <- graph %>%
  mutate(name = fct_reorder(name,-RMSE)) %>%
  ggplot(aes(x= name, y = RMSE ))+
  geom_bar(stat = 'identity',aes(fill = RMSE))+ 
  labs(title = 'RMSE', y='',x='')+ 
  scale_fill_gradient2(low='white', mid='deepskyblue1', high='navyblue')+ 
  theme(panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = "transparent"), 
        legend.box.background = element_rect(fill = "transparent"),
        axis.line.y  = element_line(colour = "black"),
        axis.ticks = element_blank(),
        text = element_text(size=15))+
  coord_flip()
ggsave(p, filename = "RMSE.png",  bg = "transparent")
#MAE monthly
graph <- data.frame( 'MAE' = rbind(ets_month_acc[2,2], arima_month_acc[2,2],
                                   arima_exog_month_acc[2,1], nnetar_exog_month_acc[2,1],
                                   nnetar_month_acc[2,2],stl_month_acc[2,2],
                                   stl_rw_month_acc[2,2], ensemble_month_acc[2]),
                     'name' = c('ETS', 'ARIMA without features','ARIMA with features',
                                'neural net 
                                with features' ,'neural net 
                                without features',
                                'STL', 'STL & random walk',
                                'ensemble'))

p <- graph %>%
  mutate(name = fct_reorder(name,- MAE)) %>%
  ggplot(aes(x= name, y = MAE ))+
  geom_bar(stat = 'identity',aes(fill = MAE))+ 
  labs(title = 'MAE', y='',x='')+ 
  scale_fill_gradient2(low='white', mid='deepskyblue1', high='navyblue')+ 
  theme(panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = "transparent"), 
        legend.box.background = element_rect(fill = "transparent"),
        axis.line.y  = element_line(colour = "black"),
        axis.ticks = element_blank(),
        text = element_text(size=15))+
  coord_flip()
ggsave(p, filename = "MAE.png",  bg = "transparent")
