##########################################################################################################
#                                      MAKE AMERICA HEALTHY AGAIN                                        #
##########################################################################################################
# 4. DATA PREPARATION FOR OPTIMIZATION                                                                   #
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

# Large cities (= cities where the placement of a hospital would be feasible)
regions_cities <- read.csv(file = "cities_2040.csv",
                           header = TRUE,
                           sep = ',')
str(regions_cities)

# Average number of inhabitants per large city for 2041-2060
avg_cities_inhabitants <- read.csv(file = "avg_inhabitants_cities.csv",
                                   header = TRUE,
                                   sep = ',')
str(avg_cities_inhabitants)

# Forecasted accidents
forecasted_accidents <- read.csv(file = 'forecasted_accidents.csv', 
                                 header = TRUE,
                                 sep = ',')
str(forecasted_accidents)

# Future accidents 
futac <- read.csv(file = 'futureAccidentDATA.csv', 
                  header = TRUE,
                  sep = ',')
str(futac)

# State abbreviations
abbreviations_states <- read.csv(file = 'abbreviations_states.csv', 
                                 skip = 1,
                                 header = FALSE,
                                 sep = ';')
str(abbreviations_states)


##### FUNCTIONS
##########################################################################################################

# Function to retrieve city from location 
f_retrieveCity <- function(city_per_region,forecasted_accidents,cities_50k,abbreviations_states){
  
  if(!require('dplyr')) { install.packages('dplyr', quietly = TRUE) }; require('dplyr', quietly = TRUE)
  library(dplyr)
  
  ## Assumption in data is made that the cities exceed 50d inhabitants in the future

  # Initiate new value
  forecasted_accidents$inCity <- 0
  forecasted_accidents$city <- NA
  forecasted_accidents$state <- NA
  forecasted_accidents$nbr_inhabitants <- NA
  
  # Add information per city
  # First: initiate empty columns
  city_per_region$State <- ''
  city_per_region$nbr_inhabitants <- 0
  city_per_region$inCity <- 0
  
  # Integer0 value
  is.integer0 <- function(x)
  {
    is.integer(x) && length(x) == 0L
  }
  
  # Merge the state in city_per_region with it's abbreviation
  city_per_region <- merge(city_per_region,abbreviations_states,by.x='State',by.y='V2',all.x=TRUE)
  city_per_region$V1 <- NULL
  
  for(i in 1:nrow(city_per_region)){
    city_name <- city_per_region[i,'City']
    if((!is.integer0(str_which(city_name,cities_50k$City))) & (!is.integer0(str_which('State',cities_50k$State)))){
      index <- str_which(city_name,cities_50k$City)
      city_per_region[i,'City'] <- cities_50k[index,]$City
      city_per_region[i,'State'] <- cities_50k[index,]$State
      city_per_region[i,'nbr_inhabitants'] <- cities_50k[index,]$Means
      city_per_region[i,'inCity'] <- 1
    }
  }
  
  # Merge based on Region number
  forecasted_accidents<-merge(forecasted_accidents,city_per_region,by.x='Region',by.y='Region',all.x=TRUE)
  #unique(merged$City)
  
  return(forecasted_accidents)
}


#  Group by raster and indicate most frequent city
f_mostFrequentCity_perBox <- function(forecasted_accidents,futac,res=0.15){
  
  if(!require('dplyr')) { install.packages('dplyr', quietly = TRUE) }; require('dplyr', quietly = TRUE)
  library(dplyr)
  
  # Initialize bboxes
  areas <- data.frame(forecasted_accidents$Region)
  names(areas) <- 'Region'
  areas$x <- as.numeric(forecasted_accidents$x)
  areas$y <- as.numeric(forecasted_accidents$y)
  areas$min_lng <- areas$x - res/2
  areas$max_lng <- areas$x + res/2
  areas$min_lat <- areas$y - res/2
  areas$max_lat <- areas$y + res/2
  
  # Determine most frequent city per bbox
  # First: determine per accident to which box it belongs
  futac$Region <- 0
  for(i in 1:nrow(futac)){
    lat <- futac[i,'Start_Lat']
    lon <- futac[i,'Start_Lng']
    if(!is.integer0(areas[which(areas$min_lat <  lat & areas$max_lat >= lat & areas$min_lng < lon & areas$max_lng > lon), ]$Region)){
      futac[i,'Region'] <- areas[which(areas$min_lat <  lat & areas$max_lat >= lat & areas$min_lng < lon & areas$max_lng > lon), ]$Region
    } else {
      futac[i,'Region'] <- NA
    }
  }  
  
  # Calculate most frequent city per region
  city_per_region <- futac %>%
    group_by(Region) %>%
    summarise(City=names(which(table(City) == max(table(City)))[1]),
              State=names(which(table(State) == max(table(State)))[1]))
  
  return(city_per_region)
}


##### Add city information to forecasts if the city has more than 50k inhabitants in 2040
##########################################################################################################

# Calculate most frequent city and state per region
city_per_region <- f_mostFrequentCity_perBox(forecasted_accidents, futac)

# Add binary incity, city name, state name and average inhabitants between 2041 and 2060
forecasted_accidents_wCity <- f_retrieveCity(city_per_region, forecasted_accidents, avg_cities_inhabitants, abbreviations_states)

# Check
sum(forecasted_accidents_wCity$incity)

# Write to CSV
write.csv(forecasted_accidents_wCity, "forecasted_accidents_wCity.csv", row.names = TRUE)

