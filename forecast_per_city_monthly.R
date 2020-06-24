### FUNCTIONS
######################################################################################################

f_create_basetable <- function(data) {
  
  # Create date object
  data$year <- format(as.Date(data$date), "%Y")
  data$month <- format(as.Date(data$date), "%m")
  data$monthyear <- paste(data$year, '-', data$month)
  
  # Create basetable
  latitudes <- aggregate(data$Start_Lat, by=list(data$City), mean)
  longitudes <- aggregate(data$Start_Lng, by=list(data$City), mean)
  dataset <- cbind(latitudes, longitudes[,2])
  colnames(dataset) <- c("City","Latitude","Longitude")
  
  if(!require('tidyr')) { install.packages('tidyr', quietly = TRUE) }; require('tidyr', quietly = TRUE)
  library(tidyr)
  
  counts <- aggregate(data$X, by=list(data$City, data$monthyear), length)
  colnames(counts) <- c("City","monthyear","count")
  counts <- spread(counts, key = "monthyear", value = "count")
  
  final_dataset <- cbind(dataset, counts[,3:(ncol(counts)-1)])
  # we start from 3rd month and end in the last month of 2039
  
  # Fill NA values with zero
  final_dataset[is.na(final_dataset)] <- 0
  
  return(final_dataset)
}

f_forecast_city_month <- function(historical_accidents) {
  
  ############################################################
  ##### Create time series per city for the next months ######
  ############################################################
  
  if(!require('forecast')) { install.packages('forecast', quietly = TRUE) }; require('forecast', quietly = TRUE)
  library(forecast)
  
  # Only use accident information
  series_df <- historical_accidents[,-(1:3)]
  
  # Library to transpose matrix
  library(data.table)
  
  # Create names for forecasting
  new_vals <- c()
  for (i in 2040:2060){
    new_vals <- c(new_vals, paste(i,'-',c(1:12)))
  }
  
  # Create empty dataframe
  forecasted_df <- data.frame()
  
  # Loop over each row
  for (i in 1:nrow(series_df)){
    trans_series <- data.frame(t(series_df[i,]))
    ts_month <- ts(data = trans_series, start = c(2036,3), end = c(2039,12), frequency = 12)
    
    # Forecast
    nn_model <- nnetar(ts_month, lamdba=0) # lamdba=0 to ensure only positive forecasts
    fc_nn <- forecast(nn_model, h=12*21)
    # transform monthly output of neural net
    fc_nn_transformed <- gather(data.frame(fc_nn), key = "month", value = "Point.Forecast")
    fc_nn_transformed$Point.Forecast <- as.numeric(fc_nn_transformed$Point.Forecast)
    
    # Create dataframe
    forecasted_df <- rbind(forecasted_df, data.frame(fc_nn_transformed)[,2])
    
  }
  
  # Assign column names
  names(forecasted_df) <- new_vals
  
  # Delete the year 2040 because that's when hospitals will be build
  forecasted_df <- forecasted_df[,-c(1:12)]
  
  # Add information about the areas to the forecasted_df
  forecasted_df$City <- historical_accidents$City
  forecasted_df$x <- historical_accidents$Latitude
  forecasted_df$y <- historical_accidents$Longitude
  
  return(forecasted_df)
}


### CREATE DATASET
######################################################################################################

basetable <- read.csv(file = "basetable.csv",
                      header = TRUE,
                      sep = ',')

final_dataset <- f_create_basetable(basetable)

# Inspect final dataset
summary(final_dataset)

# Save dataset
write.csv(final_dataset, file = "monthly_accidents_per_city.csv", row.names = TRUE)


### FORECAST
######################################################################################################

data <- read.csv(file = "monthly_accidents_per_city.csv",
                 header = TRUE,
                 sep = ',')

forecasted_accidents_monthly <- f_forecast_city_month(historical_accidents = final_dataset)
#forecasted_accidents_monthly <- f_forecast_city_month(historical_accidents = data)

# Write to CSV
write.csv(forecasted_accidents_monthly, "forecasted_accidents_monthly.csv", row.names = TRUE)


### CREATE DATASET OF CITIES WITH MORE THAN 50k INHABITANTS
######################################################################################################

cities <- read.csv(file = 'cities.csv',
                   skip = 1, # skip first row
                   header = TRUE,
                   sep = ';')
str(cities)

if(!require('dplyr')) { install.packages('dplyr', quietly = TRUE) }; require('dplyr', quietly = TRUE)
library(dplyr)

cities$X2019 = gsub(" ", "", cities$X2019) #remove spaces to make conversion to int possible
cities$X2019 = as.numeric(cities$X2019) #convert char to int 

# Select cities with more than 50 000 inhabitants
cities <- select(filter(cities, X2019 >= 50000), c(City, X2019)) 

# Split in city and state
library(stringr)
cities <- data.frame(City = str_split_fixed(cities$City, ", ", 2)[,1],
                     State = str_split_fixed(cities$City, ", ", 2)[,2],
                     Inhabitants = cities$X2019) #keep number of inhabitants for later

# Remove "city" from the city names
cities$City = str_trim(gsub("city", "", cities$City))
# Further clean city names
cities$City <- str_split_fixed(cities$City, "-", 2)[,1]
cities$City <- str_split_fixed(cities$City, "/", 2)[,1]

str(cities)


