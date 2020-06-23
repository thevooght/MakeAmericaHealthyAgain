rm(list = ls())
#dir <- paste0(getwd(), "/data")
dir <- "C:/! Project Prescriptive/data"
setwd(dir = dir)
getwd()

######################################################################################################
###################################### READ & PREPARE THE DATA #######################################
######################################################################################################

# Read in the data
forecasts <- read.csv(file = "forecasted_accidents_monthly.csv",
                      header = TRUE,
                      sep = ',')

# Sum the forecasts 
fc_accidents_per_region <- data.frame(City = forecasts$City, Latitude = forecasts$x, Longitude = forecasts$y)
fc_accidents_per_region$Forecast <- round(rowSums(forecasts[,2:241]))

# Replace negative forecasts by zero
fc_accidents_per_region$Forecast[fc_accidents_per_region$Forecast < 0] <- 0

# Inspect
summary(fc_accidents_per_region)

######################################################################################################
####################################### OPTIMIZATION ALGORITHM #######################################
######################################################################################################

### Functions
######################################################################################################

# Function to compute objective function cost
f_compute_obj_function_cost <- function(hospitals) {
  cost <- 50000000*nrow(hospitals) + sum(5000*20*hospitals$nbr_inhabitants) + sum(10*hospitals$total_distance)
  return(cost)
}

# Function to find closest hospital for a region
f_find_closest_hospital <- function(row, hospitals){
  closest_hospital_dist <- Inf
  for (i in 1:nrow(hospitals)){
    new_closest_hospital_dist <- distm(c(row$Longitude, row$Latitude), 
                                       c(hospitals[i,"Longitude"], hospitals[i,"Latitude"]), 
                                       fun = distHaversine)
    new_closest_hospital_dist <- new_closest_hospital_dist * 0.00062137
    if (new_closest_hospital_dist < closest_hospital_dist){
      closest_hospital_dist <- new_closest_hospital_dist
      closest_hospital <- c(as.character(hospitals[i,"City"]), hospitals[i,"Longitude"], hospitals[i,"Latitude"], closest_hospital_dist)
    }
  }
  return(closest_hospital)
}

### Initialization
######################################################################################################

# Create variable in dataset indicating whether that region is eligible for a hospital or not
fc_accidents_per_region$Eligible <- 0
fc_accidents_per_region[order(-fc_accidents_per_region$Forecast),][1:10,]$Eligible <- 1 
  # for now, just top X regions for testing purposes 

# Assign position of first hospital to Region with largest number of accidents (for now)
hospitals <- fc_accidents_per_region[which.max(fc_accidents_per_region$Forecast),]

# Create variable in dataset indicating whether hospital is assigned to that region or not
fc_accidents_per_region$Hospital <- 0
fc_accidents_per_region[which.max(fc_accidents_per_region$Forecast),]$Hospital <- 1
nbr_hospitals_assigned <- 1

# Extract number of inhabitants from the Region of the first hospital
hospitals$nbr_inhabitants <- 0 # TODO

# Delete assigned region from available_regions dataset
# if fcast_transformed$Hospital = 1 => region is not available anymore
fc_accidents_per_region <- fc_accidents_per_region[fc_accidents_per_region$Hospital == 0,]
  # We only need to take into account the available regions (= regions where no hospital is assigned to)
  # because the non-available regions will have a transportation cost of 0 as there is an hospital

# set new_cost = 0
new_cost <- 0

### Computing initial cost (for the scenario with one hospital)
######################################################################################################

# Compute total distance for first hospital
if(!require('geosphere')) { install.packages('geosphere', quietly = TRUE) }; require('geosphere', quietly = TRUE)
library(geosphere)

total_distance <- 0
for (i in 1:nrow(fc_accidents_per_region)){
  total_distance <- total_distance + 
    (distm(c(fc_accidents_per_region[i,"Longitude"], fc_accidents_per_region[i,"Latitude"]),
           c(hospitals$Longitude, hospitals$Latitude), 
           fun = distHaversine) # returns meters bij default
     * fc_accidents_per_region[i,"Forecast"])
}

# Convert meters to miles (1mile = 1m * 0.00062137) & save
total_distance_in_miles <- total_distance[1] * 0.00062137
hospitals[nbr_hospitals_assigned,"total_distance"] <- total_distance_in_miles

# Compute initial cost
initial_cost <- f_compute_obj_function_cost(hospitals)

### While loop
######################################################################################################

if(!require('plyr')) { install.packages('plyr', quietly = TRUE) }; require('plyr', quietly = TRUE)
library(plyr) # for "empty" function

start_time <- Sys.time() # record time needed

iter <- 0
while (new_cost < initial_cost) {
  
  if (iter != 0){ # can't do this in first iteration because initial_cost would then be assigned a zero
    # Update cost
    initial_cost <- new_cost
  }
  
  # Loop over each possible region that a new hospital can be assigned to 
  # & calculate the total cost if hospital would be assigned to that region
  eligible_regions <- fc_accidents_per_region[fc_accidents_per_region$Eligible == 1,]
  
  # Stop algorithm is there are no more eligible regions
  if (empty(eligible_regions)){
    break
  }
  
  optimal_cost <- Inf
  for (i in 1:nrow(eligible_regions)){
    
    # Place hospital in this region
    hospitals[nbr_hospitals_assigned+1,] <- eligible_regions[i,]
    hospitals[nbr_hospitals_assigned+1,]$nbr_inhabitants <- 0 # TODO
    
    # Find closest hospital for each region
    copy <- fc_accidents_per_region
    for (j in 1:nrow(copy)){
      row <- copy[j,]
      closest_hospitals <- f_find_closest_hospital(row, hospitals)
      copy[j,"Closest_hospital_city"] <- closest_hospitals[1]
      copy[j,"Closest_hospital_lng"] <- closest_hospitals[2]
      copy[j,"Closest_hospital_lat"] <- closest_hospitals[3]
      copy[j,"Closest_hospital_dist"] <- as.numeric(closest_hospitals[4])
    }
    
    # Calculate total distance (taking into account number of accidents for that region)
    copy$Total_distance <- copy$Closest_hospital_dist * copy$Forecast
    
    # Aggregate distances per city
    dist_per_hospital_agg <- aggregate(copy$Total_distance, by=list(copy$Closest_hospital_city), sum)
    colnames(dist_per_hospital_agg)<- c("City","x")
    
    # Assign distance that each hospital will need to travel in this particular allocation of hospitals
    hospitals$total_distance <- merge(hospitals, dist_per_hospital_agg, by="City")$x
    
    # Compute total cost if hospital would be allocated to this eligible region
    cost <- f_compute_obj_function_cost(hospitals)
    
    # Save allocation if it has a lower cost
    if (cost < optimal_cost){
      optimal_cost <- cost
      optimal_allocation <- hospitals[nbr_hospitals_assigned+1,]
    }
  }

  # Assign optimal allocation to result
  hospitals[nbr_hospitals_assigned+1,] <- optimal_allocation
  
  # Delete assigned region from available_regions dataset
  fc_accidents_per_region[fc_accidents_per_region$City %in% hospitals$City,]$Hospital <- 1
  fc_accidents_per_region <- fc_accidents_per_region[fc_accidents_per_region$Hospital == 0,]
  
  # Update variables
  nbr_hospitals_assigned <- nbr_hospitals_assigned + 1
  new_cost <- optimal_cost
  iter <- iter + 1
  
  # Keep track of progress
  print(iter)
  
  # Update eligible regions
  eligible_regions <- fc_accidents_per_region[fc_accidents_per_region$Eligible == 1,]
  
  print(new_cost < initial_cost)

}

# Print time needed
end_time <- Sys.time()
(end_time - start_time)
  
### Result
######################################################################################################

# Compute final objective function cost
f_compute_obj_function_cost(hospitals)

# Inspect result
str(hospitals)

# Save
write.csv(hospitals, "optimal_allocation.csv", row.names = TRUE)

