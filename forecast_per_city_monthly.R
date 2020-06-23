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
write.csv(dataset, file = "monthly_accidents_per_city", row.names = TRUE)


### FORECAST
######################################################################################################

data <- read.csv(file = "monthly_accidents_per_city.csv",
                 header = TRUE,
                 sep = ',')

forecasted_accidents_monthly <- f_forecast_city_month(historical_accidents = final_dataset)

# Write to CSV
write.csv(forecasted_accidents_monthly, "forecasted_accidents_monthly", row.names = TRUE)


### ADDING NON-ACCIDENT REGIONS
######################################################################################################

forecasted_accidents_monthly <- read.csv(file = 'forecasted_accidents_monthly', 
                                         header = TRUE,
                                         sep = ',')

f_add_nonaccident_regios <- function(forecasted_accidents, full_grid_CNT){
  #The reason for this seperate function is that it reduces the time
  #significantly while otherwise all rows would be looped in the forecast
  #to predict zero accidents.
  
  forecasted_accidents$x <- NULL #Location is already in full_grid_CNT
  forecasted_accidents$y <- NULL #Location is already in full_grid_CNT
  forecasted_accidents$X <- NULL #Index is redundant
  forecasted_accidents_allRegions <- merge(forecasted_accidents, full_grid_CNT[,-3], 
                                           by.x='Region', by.y='Var1', all.y=TRUE)
  
  #Indicate zero for regions where no accidents happened: assume that if there did 
  #not happen accidents between 2036 (week 7) and 2039 (end of the year), there will not
  #happen any in the upcoming years.
  forecasted_accidents_allRegions[is.na(forecasted_accidents_allRegions)] <- 0
  
  return(forecasted_accidents_allRegions)
}

forecasted_accidents_allRegions_monthly <- f_add_nonaccident_regios(forecasted_accidents_monthly,full_grid_CNT_monthly)

#columns x,y indicates latitude, longitude
#column Region indicates number of region

# Write to CSV
write.csv(forecasted_accidents_allRegions_monthly, "forecasted_accidents_allRegions_monthly.csv", row.names = TRUE)

