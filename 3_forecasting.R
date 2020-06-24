##########################################################################################################
#                                      MAKE AMERICA HEALTHY AGAIN                                        #
##########################################################################################################
# 3. FORECASTING                                                                                         #
##########################################################################################################
# Group 17                                                                                               #
# Regis Demolie, Cedric Devooght, Nathan De Wilde, Florian Mathieu, Jef Van Hulle                        #
##########################################################################################################

rm(list = ls())
#dir <- 'C:/! Project Prescriptive'
dir <- paste0(getwd(), "/data")
setwd(dir = dir)
getwd()


##### LOAD DATA
##########################################################################################################

# Cities
cities <- read.csv(file = "cities_cleaned.csv",
                   header = TRUE,
                   sep = ',')
str(cities)

# Future accidents per region
grid_CNT <- read.csv(file = "grid_CNT_monthly.csv",
                     header = TRUE,
                     sep = ',')
str(grid_CNT)

# Full grid (all regions included)
full_grid_CNT <- read.csv(file = "full_grid_CNT_monthly.csv", 
                          header = TRUE, 
                          sep = ',')
str(full_grid_CNT)


##### FUNCTIONS
##########################################################################################################

# Function to forecast the number of accidents for 2041-2060 per region
f_forecast_region_month <- function(historical_accidents) {
  
  ##############################################################
  ##### Create time series per region for the next months ######
  ##############################################################
  
  # Remove colums before the third month of 2036
  series_df <- historical_accidents[,-(1:5)]
  
  # Library to transpose matrix
  library(data.table)
  
  # Create names for forecasting
  new_vals <- c()
  for (i in 2040:2060){
    new_vals <- c(new_vals,paste(i,'-',c(1:12)))
  }
  
  # Create empty dataframe
  forecasted_df <- data.frame()
  
  # Loop over each row
  for (i in 1:nrow(series_df)){
    trans_series <- data.frame(t(series_df[i,]))
    ts_month <- ts(data = trans_series, start = c(2036,3), end = c(2039,12), frequency = 12)
    
    # Forecast
    nn_model  <- nnetar(ts_month, lamdba=0) # lamdba=0 to ensure only positive forecasts
    fc_nn <- forecast(nn_model, h=12*21)
    # transform monthly output of neural net
    fc_nn_transformed <- gather(data.frame(fc_nn), key = "month", value = "Point.Forecast")
    fc_nn_transformed$Point.Forecast <- as.numeric(fc_nn_transformed$Point.Forecast)
    
    # Create dataframe
    forecasted_df <- rbind(forecasted_df, data.frame(fc_nn_transformed)[,2])
    
  }
  
  names(forecasted_df) <- new_vals
  
  # Delete the year 2040 because that's when hospitals will be build
  forecasted_df <- forecasted_df[,-c(1:12)]
  
  # Add information about the areas to the forecasted_df
  forecasted_df$Region <- historical_accidents$Region
  forecasted_df$x <- historical_accidents$x
  forecasted_df$y <- historical_accidents$y
  
  return(forecasted_df)
}

# Function to calculate which cities will have more than 50k inhabitants by 2040
f_forecast_city_inhabitants <- function(cities){
  
  ### Prepare dataset
  if(!require('dplyr')) { install.packages('dplyr', quietly = TRUE) }; require('dplyr', quietly = TRUE)
  library(dplyr)
  
  # Remove cities with less than 1000 inhabitants as they will never reach 50k by 2040,
  # so we can improve calculation speed
  cities <- cities[cities$Inhabitants.2019 > 1000,]
  
  # Remove first column (= X)
  cities <- cities[,-1]
  
  # Store city and states & remove them from the dataset
  cities_and_states <- cities[,c(1:2)]
  cities <- cities[,-c(1:2)]
  

  ### Forecast
  library(forecast)
  f_forecast <- function(cities, h){
    
    forecasted_df <- data.frame()
    
    for(i in 1:nrow(cities)){
      trans_series <- data.frame(t(cities[i,]))
      names(trans_series) <- 'Freq'
      ts <- ts(data = trans_series, start = c(2010), end = c(2019), frequency = 1)
      
      # Forecast (with exponential smoothing)
      ets_model <- ets(ts, model = "ZZN")
      fc_ets <- forecast(ets_model, h = h)
      
      forecasted_df <- rbind(forecasted_df, data.frame(fc_ets)[,1])
    }
    
    return(forecasted_df)
  }
  
  
  ### Compute cities with more than 50k inhabitants in 2040

  forecasted_df_1 <- f_forecast(cities, h=21)
  names(forecasted_df_1) <- c(2020:2040)
  forecasted_df_1 <- cbind(cities_and_states, data.frame(forecasted_df_1))
    
  # Only keep last column
  cities_forecast <- data.frame(forecasted_df_1[,ncol(forecasted_df_1)])
    
  # Rename
  names(cities_forecast) <- 'forecast_2040'
  cities_forecast <- cbind(cities_and_states, data.frame(cities_forecast))
    
  # Check which columns have more than 50k inhabitants
  cities_50k <- dplyr::select(filter(cities_forecast, forecast_2040 >= 50000), c(City, forecast_2040)) 
  # Which have no 50k inhabitants:
  #cities_forecast[cities_forecast[,'forecast_2040'] < 50000,][,c('City','State')]
  
  
  ### Calculate average nbr of inhabitants for the large cities for period 2041-2060

  forecasted_df_2 <- f_forecast(cities, 41)
  names(forecasted_df_2) <- c(2020:2060)
  forecasted_df_2 <- cbind(cities_and_states, data.frame(forecasted_df_2))
    
  # Only keep last 22 columns (2041-2060) (20 years + City and State)
  cities_forecast_last20 <- data.frame(forecasted_df_2[ , (ncol(forecasted_df_2)-20):ncol(forecasted_df_2)])
  cities_forecast_last20 <- cbind(cities_and_states, cities_forecast_last20)
      
  # Check which columns have more than 50k inhabitants      
  cities_50k_inhabitants <- cities_forecast_last20[cities_forecast_last20[,'X2040'] > 50000, ]
  # Which have no 50k inhabitants:
  #cities_forecast_last20[cities_forecast_last20[,'X2040']<50000,][,c('City','State')]
      
  # Remove data from 2040
  cities_50k_inhabitants$X2040 <- NULL 
      
  # Take average from 2041 till 2060:
  avg_inhabitants <- cities_50k_inhabitants[,c('City','State')]
  avg_inhabitants$Means <- rowMeans(cities_50k_inhabitants[,-c(1:2)])
      
  cities_50k_inhabitants <- avg_inhabitants
  
  ### Return statement
  return(list(cities_50k, cities_50k_inhabitants))
}

# Function to add regions to the dataset where no accidents occurred
f_add_nonaccident_regios <- function(forecasted_accidents, full_grid_CNT){
  
  # The reason for this seperate function is that it reduces the time significantly while 
  # otherwise all rows would be looped in the forecast to predict zero accidents.
  
  forecasted_accidents$x <- NULL # Location is already in full_grid_CNT
  forecasted_accidents$y <- NULL # Location is already in full_grid_CNT
  forecasted_accidents$X <- NULL # Index is redundant
  forecasted_accidents_allRegions <- merge(forecasted_accidents, full_grid_CNT[,-3], 
                                           by.x='Region', by.y='Var1', all.y=TRUE)
  
  # Indicate zero for regions where no accidents happened: assume that if there did 
  # not happen accidents between 2036 (month 3) and 2039 (end of the year), 
  # there will not happen any in the upcoming years.
  forecasted_accidents_allRegions[is.na(forecasted_accidents_allRegions)] <- 0
  
  return(forecasted_accidents_allRegions)
}


##### FORECAST NUMBER OF ACCIDENTS PER REGION FOR 2041-2060
##########################################################################################################

# Execute predefined forecast function
forecasted_accidents_monthly <- f_forecast_region_month(historical_accidents = grid_CNT)

# Write to CSV
write.csv(forecasted_accidents_monthly, "forecasted_accidents_monthly", row.names = TRUE)


##### FORECAST NUMBER OF INHABITANTS PER CITY
##########################################################################################################

# Execute predefined forecast function
output <- f_forecast_city_inhabitants(cities)
large_cities <- output[[1]]
avg_cities_inhabitants <- output[[2]]

# Inspect final datasets
str(large_cities)
str(avg_cities_inhabitants)

# Write to csv
write.csv(large_cities, file = "cities_2040.csv", row.names = TRUE)
write.csv(avg_cities_inhabitants, file = "avg_inhabitants_cities.csv", row.names = TRUE)


### ADDING NON-ACCIDENT REGIONS
######################################################################################################

#forecasted_accidents_monthly <- read.csv(file = 'forecasted_accidents_monthly', header = TRUE, sep = ',')
#str(forecasted_accidents_monthly)

# Execute predefined function
forecasted_accidents_allRegions_monthly <- f_add_nonaccident_regios(forecasted_accidents_monthly, full_grid_CNT_monthly)

# Inspect final dataset
str(forecasted_accidents_allRegions_monthly)

#columns x,y indicates latitude, longitude
#column Region indicates number of region

# Write to CSV
write.csv(forecasted_accidents_allRegions_monthly, "forecasted_accidents_allRegions_monthly.csv", row.names = TRUE)

