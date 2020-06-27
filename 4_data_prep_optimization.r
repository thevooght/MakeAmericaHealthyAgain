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
forecasted_accidents <- read.csv(file = 'forecasted_accidents_allRegions_monthly.csv', 
                                 header = TRUE,
                                 sep = ',')
str(forecasted_accidents)

# Future accidents 
futac <- read.csv(file = 'basetable.csv', 
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

#  Group by raster and indicate most frequent city
f_mostFrequentCity_perBox <- function(forecasted_accidents, futac, res=0.15){
  
  if(!require('dplyr')) { install.packages('dplyr', quietly = TRUE) }; require('dplyr', quietly = TRUE)
  library(dplyr)
  
  # Clean dataset 1
  forecasted_accidents$X <- NULL
  # Delete rows where no accidents happened
  forecasted_accidents <-  forecasted_accidents[apply(forecasted_accidents, 1, function(row) all(row !=0 )),]
  
  # Clean dataset 2
  futac$X <- NULL
  futac$date <- as.Date(as.character(futac$date), format = '%Y-%m-%d')
  futac <- futac[(futac$date >= as.Date('01/02/2036', format='%d/%m/%Y') 
                  & futac$date <= as.Date('31/12/2039', format='%d/%m/%Y')), ]
  print("CLEAN OK")
  
  # Initialize bboxes
  areas <- data.frame(forecasted_accidents$Region)
  names(areas) <- 'Region'
  areas$x <- as.numeric(forecasted_accidents$x)
  areas$y <- as.numeric(forecasted_accidents$y)
  areas$min_lng <- areas$x - 0.15/2
  areas$max_lng <- areas$x + 0.15/2
  areas$min_lat <- areas$y - 0.15/2
  areas$max_lat <- areas$y + 0.15/2
  print("BBOXES OK")
  
  #integer0 value
  is.integer0 <- function(x){is.integer(x) && length(x) == 0L}
  
  #futac_test <- head(futac, n=5000)
  #futac_test <- tail(futac, n=5000)
  futac$Region <- 0
  for(i in 1:nrow(futac)){
    # Keep track of progress
    if (i == (nrow(futac)/4)){print("25% completed")}
    if (i == (nrow(futac)/2)){print("50% completed")}
    if (i == ((3*nrow(futac))/4)){print("75% completed")}
    lat <- futac[i,'Start_Lat']
    lon <- futac[i,'Start_Lng']
    area <- areas[which( (areas$min_lat < lat) & (areas$max_lat >= lat) & (areas$min_lng < lon) & (areas$max_lng >= lon) ), ]$Region
    if(!is.integer0(area)){
      if (length(area) > 1){
        futac[i,'Region'] <- area[1]
        print("DUBBEL AREAS")
      }
      else{
        futac[i,'Region'] <- area
      }
    }
    else{
      futac[i,'Region'] <- NA
      print("NA assigned")
    }
  }
  # Remove all rows with NA
  futac <- futac[!is.na(futac$Region),]
  print("LOOP OK")
  
  # Calculate most frequent city per region
  city_per_region <- futac %>%
    group_by(Region) %>%
    summarise(City = names(which(table(City) == max(table(City)))[1]),
              State = names(which(table(State) == max(table(State)))[1]))
  print("GROUPING OK")
  
  return(city_per_region)
}

# Function to retrieve city from location
f_retrieveCity <- function(city_per_region, forecasted_accidents, cities_50k, abbreviations_states){
  
  if(!require('dplyr')) { install.packages('dplyr', quietly = TRUE) }; require('dplyr', quietly = TRUE)
  library(dplyr)
  
  if(!require('stringr')) { install.packages('stringr', quietly = TRUE) }; require('stringr', quietly = TRUE)
  library(stringr)
  
  # Add information per city
  city_per_region$nbr_inhabitants <- 0
  city_per_region$inCity <- 0
  
  # integer0 value
  is.integer0 <- function(x){is.integer(x) && length(x) == 0L}
  
  # Merge the state in cities_50k
  cities_50k <- merge(cities_50k, abbreviations_states, by.x='State', by.y='V1', all.x=TRUE)
  cities_50k$State <- cities_50k$V2
  cities_50k$V2 <- NULL
  print("MERGING DONE")
  
  city_per_region$Results <- apply(city_per_region, 1, function(row){
    city_name <- row['City']
    state_name <- row['State']
    
    if((!is.integer0(str_which(cities_50k$City, city_name))) & (!is.integer0(str_which(cities_50k$State, state_name)))){
      # Define indexes where the city name is in the list of the cities with more than 50k inhabitants in 2040
      # & where the state name in in that state list where the city has more than 50k inhabitants in 2040 (based on the forecast)
      city_indexes <- str_which(cities_50k$City, city_name)
      state_indexes <- str_which(cities_50k$State, state_name) 
      
      # Check if the found indexes have a value which is equal, this is the place where the city and state are the same
      # assumption that no places have the same city and state name
      if(!is.integer0(intersect(city_indexes, state_indexes))){
        index <- intersect(city_indexes,state_indexes)
        city <- as.character(cities_50k[index,'City'])
        state <- as.character(cities_50k[index,'State'])
        nbr_inhabitants <- cities_50k[index,'Means']
        inCity <- 1
      } 
      else {
        city <- 'None'
        state <- 'None'
        nbr_inhabitants <- 0
        inCity <- 0
      }
    }
    else {
      city <- 'None'
      state <- 'None'
      nbr_inhabitants <- 0
      inCity <- 0
    }
    list(city,state,nbr_inhabitants,inCity)
  })
  print("ADDING RESULTS DONE")
  
  city_per_region$City <- lapply(city_per_region$Results, "[[", 1)
  city_per_region$State <- lapply(city_per_region$Results, "[[", 2)
  city_per_region$nbr_inhabitants <- lapply(city_per_region$Results, "[[", 3)
  city_per_region$inCity <- lapply(city_per_region$Results, "[[", 4)
  city_per_region$Results <- NULL
  print("ASSIGNING RESULTS DONE")
  
  # Cast columns
  city_per_region$City <- as.character(city_per_region$City)
  city_per_region$State <- as.character(city_per_region$State)
  city_per_region$nbr_inhabitants <- as.double(sapply(city_per_region$nbr_inhabitants, `[[`, 1))
  city_per_region$inCity <- as.integer(unlist(city_per_region$inCity))
  print("CASTING DONE")
  
  # Make sure that both columns are of the same type
  city_per_region$Region <- as.character(city_per_region$Region)
  forecasted_accidents$Region <- as.character(forecasted_accidents$Region)
  print("TYPE CONVERSION DONE")
  
  # Merge based on Region number
  forecasted_accidents <- merge(forecasted_accidents, city_per_region, by='Region')
  
  return(forecasted_accidents)
}

##### Add city information to forecasts if the city has more than 50k inhabitants in 2040
##########################################################################################################

# Calculate most frequent city and state per region
futac_test <- head(futac, n=10000)
futac_test <- tail(futac, n=10000)
city_per_region <- f_mostFrequentCity_perBox(forecasted_accidents, futac)

# Write to CSV
write.csv(city_per_region, "city_per_region.csv", row.names = TRUE)

# Add binary incity, city name, state name and average inhabitants between 2041 and 2060
forecasted_accidents_wCity <- f_retrieveCity(city_per_region, forecasted_accidents, avg_cities_inhabitants, abbreviations_states)

# Check
sum(forecasted_accidents_wCity$inCity)

# Write to CSV
write.csv(forecasted_accidents_wCity, "forecasted_accidents_wCity.csv", row.names = TRUE)

