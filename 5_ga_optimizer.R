##########################################################################################################
#                                      MAKE AMERICA HEALTHY AGAIN                                        #
##########################################################################################################
# 5. OPTIMIZATION                                                                                        #
##########################################################################################################
# Group 17                                                                                               #
# Regis Demolie, Cedric Devooght, Nathan De Wilde, Florian Mathieu, Jef Van Hulle                        #
##########################################################################################################

rm(list = ls())
dir <- 'C:/! Project Prescriptive'
dir <- paste0(dir, "/data")
setwd(dir = dir)
getwd()

library(GA)


##########################################################################
##                                LOAD DATA                             ##
##########################################################################

# Forecasts per region
forecasts_regions <- read.csv(file = "forecasted_accidents_allRegions_monthly.csv",
                              header = TRUE,
                              sep = ',')
str(forecasts_regions)

# City information per region
region_information <- read.csv(file = "forecasted_accidents_wCity.csv",
                              header = TRUE,
                              sep = ',')
str(region_information)


# BASE TABLE
# The basetable matrix for this optimization must look like this.
###############################################################################################
#     x      |      y      |     eligible_hospital    |     inhabitants  |  total_accidents   #
#---------------------------------------------------------------------------------------------#
#     -128   |      48.9   |        TRUE              |    59500        |     1523            #
###############################################################################################

# BASE TABLE AFTER OPTIMIZATION
# After the optimization, a new column "build_hospital" will be attached.
###############################################################################################################################################################################################
#     x      |      y      |     eligible_hospital    |   inhabitants  |  total_accidents   |   build_hospital  |    investment_cost  |   operational_cost  |  transport_cost  | total_cost   #
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#     -128   |      48.9   |         TRUE             |   59500        |     1523           |   TRUE            |     50 000 000      |      50 000 000     |  1 000 000       |  101 000 000 #     
###############################################################################################################################################################################################


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

## Create basetable matrix
f_create_basetable_matrix <- function(grid_CNT, region_info){
  region_info <- region_info[,c("Region","City","State","nbr_inhabitants","inCity")]
  base <- merge(x = grid_CNT, y = region_info, by = "Region", all.x = TRUE)
  # Some dirty cleaning
  base$X <- NULL
  base$inhabitants <- base$nbr_inhabitants
  base$nbr_inhabitants <- NULL
  # Add label indicating whether region is a suitable place to build a hospital
  base$eligible_hospital <- FALSE
  base[is.na(base)] <- 0
  base[base$inCity == 1,]$eligible_hospital <- TRUE
  # Select needed variables
  base <- base[,c("x","y","eligible_hospital","inhabitants","total_accidents")]
  return(base)
}

grid_CNT <- f_create_basetable_matrix(grid_CNT, region_information)

# For testing purposes
#grid_CNT <- grid_CNT[1:5000,]

# Inspect basetable matrix
str(grid_CNT)
summary(grid_CNT)
# There are ...  eligible places to build a hospital

##########################################################################
##  Verify whether a given dataframe grid can run through the optimizer ##
##########################################################################
f_verify_valid_table_for_optimization <- function(df_grid) {
  if(!inherits(data.frame(), "data.frame")) {
    stop("Grid table is not a data frame!")
  }
  
  cols <- colnames(df_grid)
  types <- sapply(df_grid, class)
  
  df_cols_and_types <- data.frame(cols, types)
  
  expected_cols_and_types <- cbind(c("x", "numeric"), c("y", "numeric"), c("eligible_hospital", "logical"), c("inhabitants", "numeric"), c("total_accidents", "numeric"))
  f_check <- function(col_and_type_to_check) {
    col = col_and_type_to_check[1]
    type = col_and_type_to_check[2]
    if (col %in% df_cols_and_types$cols) {
      selected_col = df_cols_and_types[df_cols_and_types$cols == col,]
      if (type %in% selected_col$types) {
        return(TRUE)
      } else {
        print(paste0("Expexted column ", col, " to have type ", type))
        return(FALSE)
      }
    } else {
      print(paste0("Expexted column ", col, " but was not present in data frame."))
      return(FALSE)
    }
  }
  checked <- apply(expected_cols_and_types, 2, f_check)
  return(sum(as.numeric(checked)) == ncol(expected_cols_and_types))
}


##########################################################################
## Precompute the cost of deliver any accident to any possible hospital ##
##########################################################################

# Generate Total Transport Cost Matrix
# rows = hospital cells, columns = all accident cells.
# For each cell in the grid, calculate the total cost of transporting 
f_generate_matrix_transport_cost <- function(df_grid) {
  ## Make local matrix of the x,y coords of the grid
  m_grid <- cbind(df_grid$x, df_grid$y)

  ## Calculate distance grid
  ## BUG: this uses the euclidian distance, and isn't actually accurate with the curverature of earth
  ## but then again, for a radius of 100 miles this likely doesn't matter.
  ## A better but very computationally expensive "improvement" would be to use haversin.
  m_distances_between_all_regions <- as.matrix(dist(m_grid, diag = TRUE, upper = TRUE))

  ## Reduce it to a matrix to only contains the distances to hospitals
  cells_in_grid_capable_of_hospital = as.integer(rownames(df_grid[df_grid$eligible_hospital == TRUE,]))
  # This is a horrible constant that approximates the radian distance to kilometers
  # It's absoluletly horrible but hey, you're asking a european to plan usa hospitals.
  # What do I care?
  cte_cost_per_kilometer = (cte_transport_cost_per_mile / 1.609344)
  cte_radian_to_kilometers_coeff = 110
  m_costs_transport <- (cte_cost_per_kilometer * cte_radian_to_kilometers_coeff) * m_distances_between_all_regions[c(cells_in_grid_capable_of_hospital),]

  # Return the cost matrix per accident of size h x c, 
  # where h is the amount of cells that can house a hospital, and c is the total amount of cells in the grid.
  return(m_costs_transport)
}

# Calculate the actual final transport costs, given a matrix that assigns
# Accident cell i to Hospital cell j.
# m_assignment must be a matrix of 1 & 0's, and have the same size as m_transport.
f_calculate_total_transport_cost_per_hospital <- function(m_transport, m_assignment) {
  if (!identical(dim(m_transport), dim(m_assignment))){
    stop("Assignment matrix and transport matrix must be of equal length!")
  } 
  
  if (!identical(as.vector(m_assignment), as.numeric(as.logical(m_assignment)))) {
    stop("Assignment matrix must only contain binary values!")
  }
  
  # Applies the m_assignment as a "mask" to the transport matrix
  # Which means only the transport costs of an accident cell to the assigned hospital cell are retrieved.
  # If a cell i is not assigned to hospital j, it receives a cost of 0.
  m_transport[as.logical(m_assignment)==0] = 0
  v_costs <- rowSums(m_transport)
  return(v_costs)
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
  
  amount_of_hospitals <- length(v_hospital_assignment)
  assigner <- function(v_accident_column) {
    # Empty assignment
    v_assignment = rep(0, amount_of_hospitals)
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

# Calculates the individual cost given that we build our hospitals at the locations indicated by v_hospital_assignment

# v_hospital_assignment: a logical vector (0 or 1's), indicating whether a hospital should be built on a cell.
# Note: to reduce the search space, the length of this vector is not all possible cells of the grid.
# But only the cells where a hospital can be built upon (rows where grid_CNT$eligible_hospital == TRUE)
f_calculate_individual_cost <- function(v_hospital_assignment) {
  # Total amount of hospitals assigned
  individual_investment_cost = cte_investment_cost_per_hospital * v_hospital_assignment
  
  # Total operational cost
  individual_operational_cost <- cte_operational_cost_per_hospital * (v_hospital_assignment * v_inhabitants_per_hospital) 
  
  # Total transport cost
  ## Optimize the assignment of accidents to hospitals
  m_assignment <- f_optimal_assignment(v_hospital_assignment, m_transport_cost_to_hospital_per_accident)
  ## Use the optimal assignment as a mask on the total transport cost matrix to only get the transport costs that occur.
  individual_transport_cost = f_calculate_total_transport_cost_per_hospital(m_total_transport_cost_for_all_accidents, m_assignment)

  # Bind all the vectors together into a matrix, 
  # each row represents a hospital, 
  # first col = investment cost
  # second col = operational cost
  # third col = transport cost
  # fourth col = total cost
  m_individual_cost = cbind(individual_investment_cost, individual_operational_cost, individual_transport_cost)
  m_individual_cost = cbind(m_individual_cost, rowSums(m_individual_cost))
  return(m_individual_cost)
}

# Aggregates the individual cost calculations into a total.
f_calculate_total_cost <- function(v_hospital_assignment) {
  # Given a vector of decisions variables (v_hospital_assignment)
  # Calculate the individual cost matrix
  m_hospital_costs <- f_calculate_individual_cost(v_hospital_assignment)
  # Fourth column is the total cost
  total_cost = -sum(m_hospital_costs[,4])
  # Turn the sum negative!
  return(total_cost)
}


##########################################################################
##                          Setup Global Environment                    ##
##########################################################################
# The setup function that precomputes constant matrices and sets constant values.
f_setup <- function(df_grid, investment_cost_per_hospital, operational_cost_per_hospital, transport_cost_per_mile) {
  # Verify that the basetable is actually suited for optimization.
  print("[*] Verifying validity of basetable/grid for optimization")
  f_verify_valid_table_for_optimization(df_grid)
  
  print("[*] Setting up constant cost values")
  cte_investment_cost_per_hospital <<- investment_cost_per_hospital
  cte_operational_cost_per_hospital <<- operational_cost_per_hospital
  cte_transport_cost_per_mile <<- transport_cost_per_mile
  
  # Precompute the transport cost for each accident to each eligible hospital
  # Matrix where each row is a hospitals cell, each column is an accident cell.
  print("[*] Precompution individual transport cost for a single accident from cell i to cell j for optimization")
  m_transport_cost_to_hospital_per_accident <<- f_generate_matrix_transport_cost(df_grid)
  
  # We must now calculate the total transport cost.
  # We turn the vector of total amount of accidents into a diagonal matrix
  # and then multiply the matrix of transport cost per accident with the diagonal matrix of total accidents.
  # This calculate the total cost of a cell with accidents to ALL hospitals!
  print("[*] Precompution individual transport cost for all accidents from cell i to cell j for optimization")
  m_total_transport_cost_for_all_accidents <<- m_transport_cost_to_hospital_per_accident %*% diag(df_grid$total_accidents)  
  
  print("[*] Storing inhabitants vector of eligible hospitals into global context")
  v_inhabitants_per_hospital <<- df_grid[df_grid$eligible_hospital == TRUE,]$inhabitants
}

##########################################################################
##                          Genetic Algorithm                           ##
##########################################################################

f_ga_optimize <- function(df_grid, investment_cost_per_hospital, operational_cost_per_hospital, transport_cost_per_mile) {
  f_setup(df_grid, investment_cost_per_hospital, operational_cost_per_hospital, transport_cost_per_mile)
  
  # v_hospital_assignment: a logical vector (0 or 1's), indicating whether a hospital should be built on a cell.
  # Note: to reduce the search space, the length of this vector is only cells with eligible_hospital == TRUE.
  f_fitness <- function(v_hospital_assignment) {
    return(f_calculate_total_cost(v_hospital_assignment))
    
  }
  
  count_of_eligible_hospitals = sum(df_grid$eligible_hospital)
  print("[*] Running Genetic Algorithm on the grid")
  GA <- ga("binary", fitness = f_fitness, maxiter = 10, run = 200, seed = 123, nBits = count_of_eligible_hospitals)
  
  # Inspect solution
  plot(GA)
  summary(GA)
  
  # Extract solution
  print(paste0("sum of solution", sum(summary(GA)$solution)))
  return(GA)
}

# Helper function that returns the cost for a given solution.
f_check_solution <- function(v_hospital_assignment, df_grid, investment_cost_per_hospital, operational_cost_per_hospital, transport_cost_per_mile) {
  f_setup(df_grid, investment_cost_per_hospital, operational_cost_per_hospital, transport_cost_per_mile)
  cost_result <- f_calculate_total_cost(v_hospital_assignment)
  return(cost_result)
}
# f_check_solution(rep(1, 699), grid_CNT, 50000000, 5000 * 20, 10)

f_ga_optimize(grid_CNT, 50000000, 5000 * 20, 10)


##########################################################################
##                            Post Processing                           ##
##########################################################################

f_build_optimal_grid <- function(v_optimal_hospital_assignment, df_grid) {
  df_grid$build_hospital = FALSE
  df_grid[df_grid$eligible_hospital == TRUE,]$build_hospital = as.logical(v_optimal_hospital_assignment)
  return(df_grid)
}

#grid_CNT <- f_add_optimal_solution_to_grid(,df_grid)

