#######################################################################
##                             Preparation                           ##
#######################################################################
rm(list = ls())

# Set working directory
#dir <- paste0(getwd(), "/data")
dir <- "C:/! Project Prescriptive/data"
setwd(dir = dir)
getwd()

# Read in the data
forecasts_regions <- read.csv(file = "forecasted_accidents_allRegions_monthly.csv",
                              header = TRUE,
                              sep = ',')

##########################################################################
## Precompute the cost of deliver any accident to any possible hospital ##
##########################################################################
library(GA)

# Generate Total Transport Cost Matrix
# rows = hospital cells, columns = all accident cells.
# For each cell in the grid, calculate the total cost of transporting 
# df_grid: pass a dataframe which contains the following columns: x, y, hospital_feasible. (grid_CNT is guuuud).
f_generate_matrix_transport_cost <- function(df_grid, cost_per_mile) {
  ## Make local matrix of the x,y coords of the grid
  m_grid <- cbind(df_grid$x, df_grid$y)

  ## Calculate distance grid
  ## BUG: this uses the euclidian distance, and isn't actually accurate with the curverature of earth
  ## but then again, for a radius of 100 miles this likely doesn't matter.
  ## A better but very computationally expensive "improvement" would be to use haversin.
  m_distances_between_all_regions <- as.matrix(dist(m_grid, diag = TRUE, upper = TRUE))

  ## Reduce it to a matrix to only contains the distances to hospitals
  #TODO: DO NOT USE $X < 700 BUT RATHER WHETHER THE CELL IN THE GRID IS ELIGIBLE TO PUT A HOSPITAL!!!
  #cells_in_grid_capable_of_hospital = as.integer(rownames(df_grid[df_grid$X < 700,]))
  cells_in_grid_capable_of_hospital = as.integer(rownames(df_grid[df_grid$Eligible == TRUE,]))
  # This is a horrible constant that approximates the radian distance to kilometers.
  # It's absoluletly horrible but hey, you're asking a european to plan usa hospitals.
  # What do I care?
  cte_cost_per_kilometer = (cost_per_mile / 1.609344)
  cte_radian_to_kilometers_coeff = 110
  m_costs_transport <- (cte_cost_per_kilometer * cte_radian_to_kilometers_coeff) * m_distances_between_all_regions[c(cells_in_grid_capable_of_hospital),]
  distances_between_all_regions <- NULL

  # Return the cost matrix per accident of size h x c, 
  # where h is the amount of cells that can house a hospital, and c is the total amount of cells in the grid.
  return(m_costs_transport)
}

## Aggregrates all the monthly accidents to a total.
f_create_aggregate_accidents_to_total <- function(df_grid) {
  v_columns_all = colnames(df_grid)
  v_columns_CNT = grep("X20", v_columns_all, value = TRUE)
  df_grid$total_accidents <- rowSums(df_grid[, v_columns_CNT])
  df_grid <- df_grid[, -which(names(df_grid) %in% v_columns_CNT)]
  # Exclude regions with no accidents (we assume these regions are no good to place a hospital)
  df_grid <- df_grid[df_grid$total_accidents > 0,]
  # Replace negative forecasts by zero (ONLY needed if we don't execute the previous statement)
  #df_grid$total_accidents[df_grid$total_accidents < 0] <- 0
  # Reset index
  rownames(df_grid) <- NULL
  return(df_grid)
}

grid_CNT <- f_create_aggregate_accidents_to_total(forecasts_regions)

# Select subset of grid_CNT for testing purposes (for the ones with 8GB RAM)
grid_CNT <- grid_CNT[1:5000,]
grid_CNT$Eligible <- FALSE
grid_CNT[1:700,]$Eligible <- TRUE

#points <- matrix(1:8, nrow = 4, ncol = 2)
m_transport_cost_to_hospital_per_accident <- f_generate_matrix_transport_cost(grid_CNT, cost_per_mile=10)

# We must now calculate the total transport cost.
# We turn the vector of total amount of accidents into a diagonal matrix,
# and then multiply the matrix of transport cost per accident with the diagonal matrix of total accidents.
# This calculates the total cost of a cell with accidents to ALL hospitals!
m_total_transport_cost_for_all_accidents <- m_transport_cost_to_hospital_per_accident %*% diag(grid_CNT$total_accidents)

# Calculate the actual final transport costs, given a matrix that assigns
# Accident cell i to Hospital cell j.
# m_assignment must be a matrix of 1 & 0's, and have the same size as m_transport.
f_calculate_total_transport_cost <- function(m_transport, m_assignment) {
  
  if (!identical(dim(m_transport), dim(m_assignment))){
    stop("Assignment matrix and transport matrix must be of equal length!")
  } 
  
  if (!identical(as.vector(m_assignment), as.numeric(as.logical(m_assignment)))) {
    stop("Assignment matrix must only contain binary values!")
  }
  
  # Applies the m_assignment as a "mask" to the transport matrix
  # Which means only the transport costs of an accident cell to the assigned hospital cell are retrieved.
  v_costs = m_transport[as.logical(m_assignment)==1]
  return(sum(v_costs))
}


f_optimal_assignment <- function(v_hospital_assignment, m_transport_cost_per_accident) {
  # Build a mask, of all enabled hospitals for each accident.
  # If a cell of a grid is not selected to build a hospital at,
  # it will be taken out of the possible places to go to.
  m_assignment_mask <- matrix(data = v_hospital_assignment, 
                              nrow = length(v_hospital_assignment), 
                              ncol = ncol(m_transport_cost_per_accident))
  
  # To eliminate non-existing hospitals from being assigned, set a insane large price to get there.
  m_transport_cost_per_accident[as.logical(m_assignment_mask)==0] = 10000000
  
  assigner <- function(v_accident_column) {
    # Empty assignment
    v_assignment = rep(0, length(v_accident_column))
    # Find location i where the hospital cost is minimum.
    # Select the first hospital we find available (in case multiple hospitals have the same cost).
    minimum <- c(min(v_accident_column))
    location_i <- which(v_accident_column == minimum, arr.ind = TRUE)
    # For this accident column, assign it to location i
    v_assignment[location_i[1]] = 1
    return(v_assignment)
  }
  
  m_assignment <- apply(m_transport_cost_per_accident, 2, assigner)
  
  # Re-apply the mask to be sure that the insane cost wasn't selected
  m_assignment[as.logical(m_assignment_mask)==0] = 0
  return(m_assignment)
}


##########################################################################
##                          Cost Calculation                            ##
##########################################################################

# Calculates the total cost given that we build our hospitals at the locations indicated by v_hospital_assignment

# v_hospital_assignment: a logical vector (0 or 1's), indicating whether a hospital should be built on a cell.
# Note: to reduce the search space, the length of this vector is not all possible cells of the grid.
# But only the cells where a hospital can be built upon (rows where grid_CNT$Eligible == TRUE)
f_calculate_total_cost <- function(v_hospital_assignment) {
  
  total_cost = 0
  
  # Total amount of hospitals assigned
  total_hospitals = sum(v_hospital_assignment)
  total_cost = -50000000 * total_hospitals
  
  # TODO: Total operational cost
  v_test_h <<- (v_hospital_assignment)
  # Total transport cost
  # Optimize the assignment of accidents to hospitals
  m_assignment <- f_optimal_assignment(v_hospital_assignment, m_transport_cost_to_hospital_per_accident)
  
  total_transport_cost = f_calculate_total_transport_cost(m_total_transport_cost_for_all_accidents, m_assignment)
  total_cost = total_cost - total_transport_cost
}



##########################################################################
##                          Genetic Algorithm                           ##
##########################################################################

# v_hospital_assignment: a logical vector (0 or 1's), indicating whether a hospital should be built on a cell.
# Note: to reduce the search space, the length of this vector is not all possible cells of the grid.
# But only the cells where a hospital can be built upon (rows where grid_CNT$Eligible == TRUE)
f_fitness <- function(v_hospital_assignment) {
  return(f_calculate_total_cost(v_hospital_assignment))
}

GA <- ga("binary", fitness = f_fitness, maxiter = 10, run = 200, seed = 123, nBits = 699)

# Inspect solution
plot(GA)
summary(GA)

# Extract solution
sum(summary(GA)$solution)
