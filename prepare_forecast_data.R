f_prepare_forecast_data <- function(data) {
  
  ### Read in data
  dat <- read.csv(file = paste0(data, ".csv"),
                      header = TRUE,
                      sep = ',')

  ### Exclude useless columns
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
  dat <- dat[ , !names(dat) %in% cols_to_delete]

  ### Handle missing values
  # 48 NA's in time -> just delete these observations
  dat <- dat[complete.cases(dat$time),]
  
  # NA's in precipation -> set 0 (under the assumption that missing value means that there was no rain)
  dat["Precipitation.in."][is.na(dat["Precipitation.in."])] <- 0
  
  # windchill has 62% missing values -> just delete this variable (? -> assumption that this would be =0 would be weird, as this can also be negative)
  dat$Wind_Chill.F.<- NULL
  
  # NA's in wind_speed, visibility, temp, humidity, pressure -> impute after splitting the data into training and test set
  
  # Replace empty strings with NA values
  dat$City[dat$City == ""] <- NA
  dat$Wind_Direction[dat$Wind_Direction == ""] <- NA
  dat$Weather_Condition[dat$Weather_Condition == ""] <- NA
  # Impute the NA values with "unknown"
  dat$City <- factor(dat$City, 
                     levels = levels(addNA(dat$City)), #addNA: add NA as a level
                     labels = c(levels(dat$City), "Unknown"), # force the NA level to be "Unknown"
                     exclude = NULL)
  dat$Wind_Direction <- factor(dat$Wind_Direction,
                               levels = levels(addNA(dat$Wind_Direction)),
                               labels = c(levels(dat$Wind_Direction), "Unknown"),
                               exclude = NULL)
  dat$Weather_Condition <- factor(dat$Weather_Condition, 
                                  levels = levels(addNA(dat$Weather_Condition)), 
                                  labels = c(levels(dat$Weather_Condition), "Unknown"), 
                                  exclude = NULL)
  # Drop unused levels ("") of the factors
  dat$City <- droplevels(dat$City)
  dat$Wind_Direction <- droplevels(dat$Wind_Direction)
  dat$Weather_Condition <- droplevels(dat$Weather_Condition)

  # Set "time" as date
  f <- "%Y-%M-%d"
  dat$date <- as.Date(as.character(dat$time), dateformat = f)
  # Remove time variable
  dat$time <- NULL 
  
  # If we manually inspect the dataset, we can see that there are 8 observations from before February 2036
  dat <- dat[order(dat$date),]
  # delete these observations
  dat <- dat[-c(1:8),] 

  return(dat)
}

basetable <- f_prepare_forecast_data(data = "futureAccidentDATA")

# Inspect basetable
summary(basetable)
str(basetable)

# Save basetable
write.csv(basetable, file = "basetable.csv")
