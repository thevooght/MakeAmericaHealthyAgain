##########################################################################################################
####################################### MAKE AMERICA HEALTHY AGAIN #######################################
################################################ Group 17 ################################################
############ Regis Demolie, Cedric Devooght, Nathan De Wilde, Florian Mathieu, Jef Van Hulle #############
##########################################################################################################


# In case you want to know what a certain variable in dataset of futureaccidents represents
# following article contains an overview:
# https://towardsdatascience.com/usa-accidents-data-analysis-d130843cde02

rm(list = ls())
#dev.off(dev.list()['RStudioGD'])
#dir <- '/Users/natha/Documents/Unief'
dir <- paste0(getwd(), "/data")
setwd(dir = dir)
getwd()


############################################### LOAD DATA ################################################
##########################################################################################################

# Accidents
futac <- read.csv(file = 'futureAccidentDATA.csv', 
                  header = TRUE,
                  sep = ',')
str(futac)

# Cities where building hospitals is okay( >= 50 000 habitants)
cities <- read.csv(file = 'cities.csv',
                   skip = 1, # skip first row
                   header = TRUE,
                   sep = ';')
str(cities)

# National holidays US
holidays <- read.csv(file = 'Public-Holiday.csv',
                     header = TRUE,
                     sep = ';')
str(holidays)

########################################## CLEAN & PREPARE DATA ##########################################
##########################################################################################################

# Required packages
if(!require('imputeMissings')) { install.packages('imputeMissings', quietly = TRUE) }; require('imputeMissings', quietly = TRUE)
if(!require('reshape2')) { install.packages('reshape2', quietly = TRUE) }; require('reshape2', quietly = TRUE)
if(!require('dplyr')) { install.packages('dplyr', quietly = TRUE) }; require('dplyr', quietly = TRUE)
if(!require('tidyquant')) { install.packages('tidyquant', quietly = TRUE) }; require('tidyquant', quietly = TRUE)

##### Cities
cities$X2019 = gsub(" ", "", cities$X2019) #remove spaces to make conversion to int possible
cities$X2019 = as.numeric(cities$X2019) #convert char to int 

library(dplyr)
cities <- select(filter(cities, X2019 >= 50000), c(City, X2019)) #remove all cities that have less than 50 000 inhabitants

library(stringr)
# Split in city and state
cities <- data.frame(City = str_split_fixed(cities$City, ", ", 2)[,1],
                     State = str_split_fixed(cities$City, ", ", 2)[,2],
                     Inhabitants = cities$X2019) #keep number of inhabitants for later

##### Holidays
# Fix date column
names(holidays)[1] <- 'Date'
holidays$Date <- as.Date(as.character(holidays$Date), format = '%d/%m/%Y')

##### Accidents
summary(futac)

# Exclude useless columns
cols_to_delete <- c("X", "ID", "Source", "TMC", "Severity", "End_Lat", "End_Lng", "Distance.mi.", "Description", 
                    "Number", "Street", "Side", "Country", "Timezone", "Airport_Code", "Zipcode",
                    "Weather_Timestamp",
                    "Amenity", "Bump", "Crossing", "Give_Way", "Junction", "No_Exit", "Railway", "Roundabout",
                    "Station", "Stop", "Traffic_Calming", "Traffic_Signal", "Turning_Loop",
                    "Sunrise_Sunset", "Civil_Twilight", "Nautical_Twilight", "Astronomical_Twilight") 
  # only include start_lat & start_lng as coordinate variables (end_lat & end_lng has 75% missing values) => assume the start coordinates are accurate enough
  # don't need address (number, street, side) as we base our calculations on the coordinates
  # exclude binary columns (but ?IDEA?: calculate number of TRUE values for particular area (however probably too little true's))
  # don't need information about whether is was night or day
basetable <- futac[ , !names(futac) %in% cols_to_delete]

# What to do with NAs
colSums(is.na(basetable))
summary(basetable)

# 48 NA's in time -> just delete these observations
basetable <- basetable[complete.cases(basetable$time),]

# NA's in precipation -> set 0 (under the assumption that missing value means that there was no rain)
basetable["Precipitation.in."][is.na(basetable["Precipitation.in."])] <- 0

# windchill has 62% missing values -> just delete this variable (? -> assumption that this would be =0 would be weird, as this can also be negative)
basetable$Wind_Chill.F.<- NULL

# NA's in wind_speed, visibility, temp, humidity, pressure -> impute
library(imputeMissings)
cols_to_impute <- c("Temperature.F.", "Humidity...", "Pressure.in.", "Visibility.mi.", "Wind_Speed.mph.")
basetable[cols_to_impute] <- imputeMissings::impute(basetable[cols_to_impute])
  # numeric/integer vectors are imputed with the median

# There are also some missing values for wind_direction, weather_condition, & city (when manually inspecting the dataset)
table(basetable$City[basetable["City"] == ""])[1]
table(basetable$City[basetable["Wind_Direction"] == ""])[1]
table(basetable$City[basetable["Weather_Condition"] == ""])[1]
# Replace the empty strings with NA values
basetable$City[basetable$City == ""] <- NA
basetable$Wind_Direction[basetable$Wind_Direction == ""] <- NA
basetable$Weather_Condition[basetable$Weather_Condition == ""] <- NA
# Impute the NA values with "unknown"
basetable$City <- factor(basetable$City, 
                         levels = levels(addNA(basetable$City)), #addNA: add NA as a level
                         labels = c(levels(basetable$City), "Unknown"), # force the NA level to be "Unknown"
                         exclude = NULL)
basetable$Wind_Direction <- factor(basetable$Wind_Direction, 
                                   levels = levels(addNA(basetable$Wind_Direction)),
                                   labels = c(levels(basetable$Wind_Direction), "Unknown"),
                                   exclude = NULL)
basetable$Weather_Condition <- factor(basetable$Weather_Condition, 
                                      levels = levels(addNA(basetable$Weather_Condition)), 
                                      labels = c(levels(basetable$Weather_Condition), "Unknown"), 
                                      exclude = NULL)
# Drop unused levels ("") of the factors
basetable$City <- droplevels(basetable$City)
basetable$Wind_Direction <- droplevels(basetable$Wind_Direction)
basetable$Weather_Condition <- droplevels(basetable$Weather_Condition)

# Check NA's
colSums(is.na(basetable))
str(basetable)

# Set "time" as date
f <- "%Y-%M-%d"
basetable$date <- as.Date(as.character(basetable$time), dateformat = f)
basetable$time <- NULL

# If we manually inspect the dataset, we can see that there are 8 observations from before February 2036
basetable <- basetable[order(basetable$date),]
# delete these observations
basetable <- basetable[-c(1:8),] 

# Inspect final dataset
str(basetable)


##################################### SPATIAL GRID FOR ALL ACCIDENTS #####################################
######################### Calculate number of accidents per geographical raster ##########################
##########################################################################################################

# Required packages
if(!require('raster')) { install.packages('raster', quietly = TRUE) }; require('raster', quietly = TRUE)
if(!require('rasterVis')) { install.packages('rasterVis', quietly = TRUE) }; require('rasterVis', quietly = TRUE)
if(!require('sp')) { install.packages('sp', quietly = TRUE) }; require('sp', quietly = TRUE)

library(raster)
library(rasterVis)
library(RColorBrewer)
library(sp) #for SpatialPoints

# Define dimensions & res size
lon_min <- -128.0; lon_max <- -65.0; lat_min <- 25.5; lat_max <- 50.5
res <- 0.1 #degrees

# Create copy of basetable: data
data <- basetable
data$area_ha <- 0.000004 #ha=1km^2 and surface of a vehicle is e.g. 4m^2
coords <- cbind(data$Start_Lng, data$Start_Lat)
data_pts <- SpatialPointsDataFrame(coords=coords, data=data.frame(data$area_ha))
names(data_pts) <- "area_ha"

# Create (empty) rasters
cell_size <- 0.25 #500m x 500m
ncols <- ((lon_max - lon_min)/cell_size)+1
nrows <- ((lat_max - lat_min)/cell_size)+1 
accident_counts <- raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, ymn=lat_min, ymx=lat_max, res=res, crs="+proj=longlat +datum=WGS84")
accident_counts[] <- 0
accident_counts

# rasterize: transfer values associated with spatial data to raster cells
# in other words, calculate number of accidents per raster cell
accident_counts <- rasterize(coords, accident_counts, fun = "count")
accident_counts

# Plot map
plot(log10(accident_counts), col=brewer.pal(9,"BuPu"), sub="log10 Number of Accidents")

# Create table for time series
table <- data.frame(table(cellFromXY(accident_counts, data_pts))) 
  #should have 63 |-65--128| times 25 |50.5-25.5| times 100 (0.1 is 1/100 of 1) squared degrees or 157500
head(table) #note that several squares do not have values for several reasons: no accidents due to no streets or located in sea

# Adding left out squares with value zero
grid_count <- data.frame(rasterToPoints(accident_counts, spatial = F))
grid_count$Var1 <- table$Var1


######################################### CREATE FINAL BASETABLE #########################################
##########################################################################################################

###### Save final basetable
write.csv(basetable, file = "basetable.csv")


############################################## BENCHMARKING ############################################## 
##########################################################################################################


# Required packages
if(!require('forecast')) { install.packages('forecast', quietly = TRUE) }; require('forecast', quietly = TRUE)

# Load basetable
basetable <- read.csv(file = "basetable.csv")
#basetable$X <- NULL

# Aggregate accidents over all geographical rosters per date
train_agg <- aggregate(basetable$X, by=list(Date=basetable$date), length)
  # ! rosters not yet available (just aggregate per city for now)
names(train_agg)[2] <- 'Freq'
train_agg$Date <- as.Date(train_agg$Date)
identical(sum(train_agg$Freq), nrow(basetable)) #has to be true -> ok

summary(train_agg) #start period on 8th of February

# Some problems can arise when forecasting on a low periodicity.
# In general, the lower the periodicity, the less accurate the forecast will be.
# KISS-principle: only create low-level forecasts if truly needed
# source: https://www.forecastpro.com/wp-content/uploads/2019/03/WebinarDailyWeeklyData.pdf

# Let's benchmark both weekly and monthly and then choose 1 to forecast

##### MAKE OUT-OF-PERIOD SAMPLE

# Train set : 2036-2038 / Test set: 2039
indTRAIN <- as.Date(as.Date("2036-02-08", dateformat = f, origin = "1970-01-01") : as.Date("2038-12-31", origin = "1970-01-01"), dateformat = f, origin = "1970-01-01")
indTEST <- as.Date(as.Date("2039-01-01", dateformat = f, origin = "1970-01-01") : as.Date("2039-12-31", origin = "1970-01-01"), dateformat = f, origin = "1970-01-01")

# Not sure if for this next step, giving missing days frequency=0 is absolutely necessary, but done anyways (might happen automatically)
  # EDIT: not necessary since we aggregate per week later (I suppose)

# How many days are we training?
length(train_agg$Freq[train_agg$Date %in% indTRAIN]) #1054 != length of indTrain 

# This means that there are dates with 0 accidents -> put them in train_agg for completeness?
#Missing <- indTRAIN[!indTRAIN %in% train_agg$Date] 
#Missing 
#Missing <- as.data.frame(Missing)
#Missing$Freq <- 0
#names(Missing)[1] <- 'Date'
#train_agg <- rbind(train_agg, Missing)
#train_agg <- train_agg[order(train_agg$Date),]

# How many days are we testing? 
length(train_agg$Freq[train_agg$Date %in% indTEST]) #365 ->ok

##### CREATE TIME-SERIES
# WEEKLY
library(tidyquant)

train_week <- train_agg %>% tq_transmute(select = Freq,
                                         mutate_fun = apply.weekly,
                                         FUN = sum)
train_week$Date <- as.Date(train_week$Date)
train_week <- train_week[-1,] #start from full first week

# How many weeks are we training?
length(train_week$Freq[train_week$Date %in% indTRAIN])#->150
# How many weeks are we testing? 
length(train_week$Freq[train_week$Date %in% indTEST]) #->52 


train_ts_week <- ts(data = train_week$Freq[train_week$Date %in% indTRAIN],
                 start = c(2036,7), end = c(2038,52), frequency = 52)

test_ts_week <- ts(data = train_week$Freq[train_week$Date %in% indTEST],
              start = c(2039,1), end = c(2039,52), frequency = 52)

# MONTHLY
train_month <- train_agg %>% tq_transmute(select = Freq,
                                          mutate_fun = apply.monthly,
                                          FUN = sum)
train_month$Date <- as.Date(train_month$Date)
#train_month <- train_month[-c(1:3),] #start from full first month

# How many months are we training?
length(train_month$Freq[train_month$Date %in% indTRAIN])#34
# How many months are we testing? 
length(train_month$Freq[train_month$Date %in% indTEST]) #12

train_ts_month <- ts(data = train_month$Freq[train_month$Date %in% indTRAIN],
                    start = c(2036,3), end = c(2038,12), frequency = 12)

test_ts_month <- ts(data = train_month$Freq[train_month$Date %in% indTEST],
                   start = c(2039,1), end = c(2039,12), frequency = 12)

##### BENCHMARK FORECASTING METHODS

##### 1) ets

ets_model <- ets(train_ts_week, model = "ZZN")
forecast_ets_week <- forecast(ets_model,h=52)
plot(forecast_ets_week)

ets_model <- ets(train_ts_month, model = "ZZN")
forecast_ets_month <- forecast(ets_model,h=12)
plot(forecast_ets_month)

##### 2) Arima with external regressors

###create features table
features <- new[,-c(1:3)]
features_agg <- aggregate(features, by = list(Date = features$date), mean)

#WEEKLY
features_week <- features_agg %>%
  tq_transmute(select     = c(Temperature.F.,Humidity...,Pressure.in.,Visibility.mi.,Wind_Speed.mph., Precipitation.in.),
               mutate_fun = apply.weekly,
               FUN        = mean)
features_week$Date <- as.Date(features_week$Date)
#also add a logical for whether there was a holiday that week (won't do it for month, seems not useful in that case)
nextweekday <- function(date, wday) {   #function to get the next sunday
  date <- as.Date(date)
  diff <- wday - wday(date)
  diff=if_else(diff<0,diff+7,diff)
  return(date + diff)
}
Holidays$Date <- nextweekday( Holidays$Date ,1)
features_week$HasHoliday <- ifelse(features_week$Date %in% Holidays$Date, 1, 0 )
  
#Split in train and test
features_weekTRAIN <- features_week[features_week$Date %in% indTRAIN,]
features_weekTRAIN <- features_weekTRAIN[-c(1),] #was excluded from train, not a full week
features_weekTEST <- features_week[features_week$Date %in% indTEST, ]


#Arima model with external regressors, week
arima_model_exog  <- auto.arima(train_ts_week, approximation = FALSE, stepwise = TRUE, xreg = as.matrix(features_weekTRAIN[,-1]))
forecast_arima_exog_week <- forecast(arima_model_exog,h=52,xreg = as.matrix(features_weekTEST[,-1]))
plot(forecast_arima_exog_week)

#MONTHLY
features_month <- features_agg %>%
  tq_transmute(select     = c(Temperature.F.,Humidity...,Pressure.in.,Visibility.mi.,Wind_Speed.mph., Precipitation.in.),
               mutate_fun = apply.monthly,
               FUN        = mean)
features_month$Date <- as.Date(features_month$Date)

#Split in train and test
features_monthTRAIN <- features_month[features_month$Date %in% indTRAIN,]
features_monthTRAIN <- features_monthTRAIN[-c(1),] #was excluded from train, not a full month
features_monthTEST <- features_month[features_month$Date %in% indTEST, ]

#Arima model with external regressors, week
arima_model_exog  <- auto.arima(train_ts_month, approximation = FALSE, stepwise = TRUE, xreg = as.matrix(features_monthTRAIN[,-1]))
forecast_arima_exog_month <- forecast(arima_model_exog,h=12,xreg = as.matrix(features_monthTEST[,-1]))
plot(forecast_arima_exog_month)

##### 3) NN with external regressors

#Neural net with external regressors
#WEEKLY
nnetar_model_exog  <- nnetar(train_ts_week, p = 1, P =1, xreg = as.matrix(features_weekTRAIN[,-1]))
forecast_nnetar_exog_week <- forecast(nnetar_model_exog,h=52,xreg = as.matrix(features_weekTEST[,-1]))
plot(forecast_nnetar_exog_week)
#MONTHLY
nnetar_model_exog  <- nnetar(train_ts_month, p = 1, P =1, xreg = as.matrix(features_monthTRAIN[,-1]))
forecast_nnetar_exog_month <- forecast(nnetar_model_exog,h=12,xreg = as.matrix(features_monthTEST[,-1]))
plot(forecast_nnetar_exog_month)

##### 4) ensemble
#WEEKLY
forecast_ensemble_week <- data.frame(Forecast_ENS = rowMeans(cbind(data.frame(forecast_ets_week)[1], 
                                                              data.frame(forecast_arima_exog_week)[1], 
                                                              data.frame(forecast_nnetar_exog_week)[1])))
#MONTHLY
#issue in forecast of nnetar -> had to make adjustments(don't know why)
Neural_month <- as.data.frame(t(data.frame(forecast_nnetar_exog_month)))
rownames(Neural_month) <- NULL
names(Neural_month)[1] <- 'Point.Forecast'
Neural_month$Point.Forecast <- as.double(Neural_month$Point.Forecast)

forecast_ensemble_month <- data.frame(Forecast_ENS = rowMeans(cbind(data.frame(forecast_ets_month)[1], 
                                                                   data.frame(forecast_arima_exog_month)[1], 
                                                                   data.frame(Neural_month))))
 

#################### Evaluate performance on out-of-sample set######################
#In order to calculate the performance of the ensemble, first transform to a TS
#WEEKLY
forecast_ensemble_week <- ts(forecast_ensemble_week$Forecast_ENS, 
                        start = c(2039,1), end = c(2039,52),
                        frequency = 52)
forecast_ensemble_month <- ts(forecast_ensemble_month$Forecast_ENS, 
                             start = c(2039,1), end = c(2039,12),
                             frequency = 12)

#RMSE and MAE are scale dependent errors, wheras MAPE and MASE is scale-independent
#SO: use RMSE and MAE to compare monthly and weekly each, MAPE can be used to compare weekly and monthly(!!)
#weekly
forecast::accuracy(object = forecast_ets_week, x = test_ts_week)[,c("RMSE","MAE", "MAPE")]
forecast::accuracy(object = forecast_arima_exog_week, x = test_ts_week)[,c("RMSE","MAE", "MAPE")]
forecast::accuracy(object = forecast_nnetar_exog_week, x = test_ts_week)[,c("RMSE","MAE", "MAPE")]
forecast::accuracy(object = forecast_ensemble_week, x = test_ts_week)[,c("RMSE","MAE", "MAPE")]

#monthly
forecast::accuracy(object = forecast_ets_month, x = test_ts_month)[,c("RMSE","MAE", "MAPE")]
forecast::accuracy(object = forecast_arima_exog_month, x = test_ts_month)[,c("RMSE","MAE", "MAPE")]
forecast::accuracy(object = forecast_nnetar_exog_month, x = test_ts_month)[,c("RMSE","MAE", "MAPE")]
forecast::accuracy(object = forecast_ensemble_month, x = test_ts_month)[,c("RMSE","MAE", "MAPE")]


#next Step
#Use best model to forecast per state/city/county.. and date(month/week)


