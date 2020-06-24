f_accidents_forecast <- function(historical_accidents, years_to_forecast) {

  # Read in data
  dat <- read.csv(file = paste0(historical_accidents, ".csv"),
                  header = TRUE,
                  sep = ',')

  # Create weekly time-series & split in training and test set
  train_ts <- ts(data = dat, start = c(2036,7), end = c(2040,52), frequency = 52)
  test_ts <- ts(data = NA, start = c(2041,1), end = c(2060,52), frequency = 52)
  
  if(!require('forecast')) { install.packages('forecast', quietly = TRUE) }; require('forecast', quietly = TRUE)
  library(forecast)

  # Neural Net
  fc_nnetar_week <- forecast(nnetar(dat), h=52)
  #plot(fc_nnetar_week)

  
  return(forecasted_accidents)
}

# CODE FLORIAN:
f_accidents_forecast <- function(historical_accidents, year_to_forecast) {
  forecasted_accidents = data.frame()
  
  ## Forecast the accidents
  
  ## Estimate the amount of accidents for each year
  
  ## Build a grid (hexagon? is nice because allows you to store just center point (lat/lng) and then a size parameter)
  
  ## Using the historical accidents, estimate the probability that an accident happens in a particular grid.
  
  ## Draw a random grid based on the probabilities for each forecasted accident
  
  ## (Pick a random point within the grid to assign to the accident)
  return(forecasted_accidents)
}