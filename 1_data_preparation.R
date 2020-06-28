##########################################################################################################
#                                      MAKE AMERICA HEALTHY AGAIN                                        #
##########################################################################################################
# 1. DATA CLEANING & PREPARATION                                                                          #
##########################################################################################################
# Group 17                                                                                               #
# Regis Demolie, Cedric Devooght, Nathan De Wilde, Florian Mathieu, Jef Van Hulle                        #
##########################################################################################################

# The following article contains an overview of the meaning behind the variables in the futureaccidents dataset:
# https://towardsdatascience.com/usa-accidents-data-analysis-d130843cde02

rm(list = ls())
#dir <- 'C:/! Project Prescriptive'
dir <- paste0(getwd(), "/data")
setwd(dir = dir)
getwd()


##### LOAD DATA
##########################################################################################################

# Future accidents
futac <- read.csv(file = "futureAccidentDATA.csv",
                  header = TRUE,
                  sep = ',')
str(futac)

# Cities
library("readxl")
cities <- read_excel("SUB-IP-EST2019-ANNRES.xlsx", col_names = TRUE, col_types = NULL, na = '', skip = 3)
str(cities)


##### FUNCTIONS
##########################################################################################################

# Function to clean the futureAccidentDATA set
f_prepare_accidents_data <- function(data) {
  
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
  data <- data[ , !names(data) %in% cols_to_delete]

  ### Handle missing values
  # 48 NA's in time -> just delete these observations
  data <- data[complete.cases(data$time),]
  
  # NA's in precipation -> set 0 (under the assumption that missing value means that there was no rain)
  data["Precipitation.in."][is.na(data["Precipitation.in."])] <- 0
  
  # windchill has 62% missing values -> just delete this variable (? -> assumption that this would be =0 would be weird, as this can also be negative)
  data$Wind_Chill.F.<- NULL
  
  # NA's in wind_speed, visibility, temp, humidity, pressure -> impute after splitting the data into training and test set
  
  # Replace empty strings with NA values
  data$City[data$City == ""] <- NA
  data$Wind_Direction[data$Wind_Direction == ""] <- NA
  data$Weather_Condition[data$Weather_Condition == ""] <- NA
  # Impute the NA values with "unknown"
  data$City <- factor(data$City, 
                     levels = levels(addNA(data$City)), #addNA: add NA as a level
                     labels = c(levels(data$City), "Unknown"), # force the NA level to be "Unknown"
                     exclude = NULL)
  data$Wind_Direction <- factor(data$Wind_Direction,
                               levels = levels(addNA(data$Wind_Direction)),
                               labels = c(levels(data$Wind_Direction), "Unknown"),
                               exclude = NULL)
  data$Weather_Condition <- factor(data$Weather_Condition, 
                                  levels = levels(addNA(data$Weather_Condition)), 
                                  labels = c(levels(data$Weather_Condition), "Unknown"), 
                                  exclude = NULL)
  # Drop unused levels ("") of the factors
  data$City <- droplevels(data$City)
  data$Wind_Direction <- droplevels(data$Wind_Direction)
  data$Weather_Condition <- droplevels(data$Weather_Condition)

  # Set "time" as date
  f <- "%Y-%M-%d"
  data$date <- as.Date(as.character(data$time), dateformat = f)
  # Remove time variable
  data$time <- NULL 
  
  # If we manually inspect the dataset, we can see that there are 8 observations from before February 2036
  data <- data[order(data$date),]
  # delete these observations
  data <- data[-c(1:8),] 

  return(data)
}

# Function to clean the cities dataset
f_prepare_cities_data <- function(cities){
  
  # Convert tibble to dataframe
  cities <- as.data.frame(cities)
  
  # Fill in missing values with zero
  cities[is.na(cities)] <- 0
  
  # Needed packages
  if(!require('dplyr')) { install.packages('dplyr', quietly = TRUE) }; require('dplyr', quietly = TRUE)
  library(dplyr)
  
  # Delete census and estimates.base column
  cities <- cities[,-c(2:3)]
  
  # Split in city and state
  library(stringr)
  cities <- data.frame(City = str_split_fixed(cities$`...1`, ", ", 2)[,1],
                       State = str_split_fixed(cities$`...1`, ", ", 2)[,2],
                       Inhabitants = cities[,2:ncol(cities)]) #keep number of inhabitants for later
  
  # Remove "city" from the city names
  cities$City = str_trim(gsub("city", "", cities$City))
  
  # Further clean city names
  cities$City <- str_split_fixed(cities$City, "-", 2)[,1]
  cities$City <- str_split_fixed(cities$City, "/", 2)[,1]
  
  return(cities)
}

# Function to create a dataset with number of accidents per region
# Based on the future accidents dataset
f_accidents_basetable_region_month <- function(res, futac){
  
  ##############################################
  ####### Spatial grid for all accidents #######
  ##############################################
  
  if(!require('raster')) { install.packages('raster', quietly = TRUE) }; require('raster', quietly = TRUE)
  if(!require('rasterVis')) { install.packages('rasterVis', quietly = TRUE) }; require('rasterVis', quietly = TRUE)
  if(!require('sp')) { install.packages('sp', quietly = TRUE) }; require('sp', quietly = TRUE)
  if(!require('stringr')) { install.packages('stringr', quietly = TRUE) }; require('stringr', quietly = TRUE)
  if(!require('dplyr')) { install.packages('dplyr', quietly = TRUE) }; require('dplyr', quietly = TRUE)
  
  library(raster)
  library(rasterVis)
  library(RColorBrewer)
  library(sp) #for SpatialPoints
  library(stringr)
  library(dplyr)
  
  # Define dimensions
  lon_min <- -128.0; lon_max <- -65.0; lat_min <- 25.5; lat_max <- 50.5
  # Define res size
  # res = 0.15 #degrees #default in function
  
  futac$area_ha <- 0.000004 #ha=1km^2 and surface of a vehicle is e.g. 4m^2 #value can be any number.
  futac_coords <- cbind(futac$Start_Lng, futac$Start_Lat)
  futac_pts <- SpatialPointsDataFrame(coords=futac_coords, data=data.frame(futac$area_ha)) #dataframe only needs to have the same nrow as futac_coords
  names(futac_pts) <- "area_ha"
  
  # Create (empty) rasters
  cell_size <- 0.25 
  ncols <- ((lon_max - lon_min)/cell_size)+1
  nrows <- ((lat_max - lat_min)/cell_size)+1 
  accident_counts <- raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, ymn=lat_min, ymx=lat_max, res=res, crs="+proj=longlat +datum=WGS84")
  accident_counts[] <- 0
  
  accident_area <- raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, ymn=lat_min, ymx=lat_max, res=res, crs="+proj=longlat +datum=WGS84")
  
  # rasterize: transfer values associated with spatial data to raster cells
  # in other words, calculate number of accidents per raster cell
  accident_counts <- rasterize(futac_coords, accident_counts, fun="count")
  full_raster <- rasterize(futac_coords, accident_counts, fun="count", background=0)
  
  # Plot map
  #plot(log10(accident_counts), col=brewer.pal(9,"BuPu"), sub="log10 Number of Accidents")
  
  # Create table to use in a time series
  table <- data.frame(table(cellFromXY(accident_counts, futac_pts))) 
  #should have 63 |-65--128| times 25 |50.5-25.5| times 100 (0.1 is 1/100 of 1) squared degrees or 157500
  head(table) # note that several squares do not have values for several reasons: no accidents due to no streets or located in sea
  
  # Adding left out squares with value zero
  grid_count <- data.frame(rasterToPoints(accident_counts, spatial = F))
  grid_count$Var1 <- table$Var1
  
  grid_full_count <- data.frame(rasterToPoints(full_raster, spatial = F))
  grid_full_count$Var1 <- 1:nrow(grid_full_count)
  
  
  ##############################################################
  ####### Creating table of accidents per area per month #######
  ##############################################################
  
  # Create list with year - month
  val <- c()
  for (i in 2036:2039){
    val <- c(val, paste(i,'-',c(1:12)))
  }
  
  grid_CNT <- grid_count[,-3]
  names(grid_CNT)[3] <- "Region"
  
  # Create date object
  futac$date <- format(as.POSIXct(futac$time, format='%Y-%m-%d %H:%M:%S'), format='%m/%d/%Y')
  futac$date <- as.Date(futac$date, format='%m/%d/%Y')
  
  futac_divided <- futac
  futac$year <- format(as.Date(futac$date), "%Y")
  futac$month <- format(as.Date(futac$date), "%m")
  futac$month <- str_remove(futac$month, "^0+")
  futac$monthyear <- paste(futac$year,'-',futac$month)
  
  for (i in val){
    df_new <- subset(futac[,c("Start_Lat", "Start_Lng", "monthyear")], monthyear == i)
    
    #df_new might be empty, because sometimes there were no accidents in the corresponding val (year-month)
    if (nrow(df_new) != 0){
      
      df_new$area_ha <- 0.000004
      df_new_coords <- cbind(df_new$Start_Lng, df_new$Start_Lat)
      df_new_pts <- SpatialPointsDataFrame(coords=df_new_coords, data=data.frame(df_new$area_ha))
      names(df_new_pts) <- "area_ha"
      
      # create (empty) rasters
      cell_size <- 0.25 #500m x 500m
      ncols <- ((lon_max - lon_min)/cell_size)+1
      nrows <- ((lat_max - lat_min)/cell_size)+1 
      accident_counts <- raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, ymn=lat_min, ymx=lat_max, res=res, crs="+proj=longlat +datum=WGS84")
      accident_counts[] <- 0
      
      accident_area <- raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, ymn=lat_min, ymx=lat_max, res=res, crs="+proj=longlat +datum=WGS84")
      
      # rasterize
      accident_counts <- rasterize(df_new_coords, accident_counts, fun="count")
      
      # Create table to use in a time series
      table <- data.frame(table(cellFromXY(accident_counts, df_new_pts))) 
      
      # Adding left out squares with value zero
      grid_count <- data.frame(rasterToPoints(accident_counts, spatial = F))
      grid_count$Var1 <- table$Var1
      
      grid_count[1] <- grid_count[2] <- NULL
      
      # Merge the new grid_count with the already existing grid_CNT
      # Purpose is adding counts per region of the new month (month equal to val)
      grid_CNT <- merge(grid_CNT, grid_count %>% dplyr::select('Var1', 'layer'), by.x = 'Region', by.y = 'Var1', all.x=TRUE )
      names(grid_CNT)[names(grid_CNT) == "layer"] <- paste('CNT_',i)
      grid_CNT$Var1 <- NULL
      
      # Indicate zero value where there were no accidents (where the merge indicated NA)
      grid_CNT[is.na(grid_CNT)] <- 0
    }
    
    else{
      grid_CNT$newcol <- 0
      names(grid_CNT)[names(grid_CNT) == "newcol"] <- paste('CNT_',i)
    }
  }
  
  return(list(grid_CNT,grid_full_count))
}

# Function to prepare the data for exploration
f_prepare_data_shiny <- function(data){
  result <- aggregate(X~City, data, length)
  result$long <- aggregate(Start_Lng~City, data, mean)[,2]
  result$lat <- aggregate(Start_Lat~City, data, mean)[,2]
  return(result)
}

##### CLEAN FUTURE ACCIDENTS DATASET
##########################################################################################################

# Execute predefined function
basetable <- f_prepare_accidents_data(data = futac)

# Inspect basetable
summary(basetable)
str(basetable)

# Save basetable
write.csv(basetable, file = "basetable.csv")


##### CLEAN CITIES DATASET
##########################################################################################################

# Look at the data
str(cities)
summary(cities)

# Execute predefined function
cities_cleaned <- f_prepare_cities_data(cities = cities)

# Plot general evolution of number of inhabitants
evolution <- colSums(as.data.frame(cities_cleaned[,-c(1:2)]))
plot(evolution, type="l", xlab="years", ylab="inhabitants", lwd=2, main="Evolution population growth USA")

# Inspect final dataset
str(cities_cleaned)

# Write to csv
write.csv(cities_cleaned, file = "cities_cleaned.csv", row.names = TRUE)


### CREATE ACCIDENTS PER REGION DATASET
######################################################################################################

# Execute predefined function
output <- f_accidents_basetable_region_month(res = 0.15, futac = futac)
grid_CNT_monthly <- output[[1]]
full_grid_CNT_monthly <- output[[2]]

# Save datasets
write.csv(grid_CNT_monthly, file = "grid_CNT_monthly.csv", row.names = TRUE)
write.csv(full_grid_CNT_monthly, file = "full_grid_CNT_monthly.csv", row.names = TRUE)

### PREPARE CITY DATA
######################################################################################################

# Execute predefined function
basetable_agg <- f_prepare_data_shiny(data = accidents)

# Inspect basetable
summary(basetable_agg)
str(basetable_agg)

# Save basetable
write.csv(basetable_agg, file = "basetable_agg.csv")
