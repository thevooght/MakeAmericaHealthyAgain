library(GA)

# The basetable matrix for this optimization must look like this.
###############################################################################################
#     x      |      y      |     eligible_hospital    |     inhabitants  |  total_accidents   #
#---------------------------------------------------------------------------------------------#
#     -128   |      48.9   |        TRUE              |    59500        |     1523            #
###############################################################################################

# After the optimization, a new column "build_hospital" will be attached.
###############################################################################################################################################################################################
#     x      |      y      |     eligible_hospital    |   inhabitants  |  total_accidents   |   build_hospital  |    investment_cost  |   operational_cost  |  transport_cost  | total_cost   #
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#     -128   |      48.9   |         TRUE             |   59500        |     1523           |   TRUE            |     50 000 000      |      50 000 000     |  1 000 000       |  101 000 000 #     
###############################################################################################################################################################################################

# Helper function: convert grid to one that matches.
## Aggregrates all the weekly CNT_'s to total_accidents
f_create_aggregate_accidents_to_total <- function(df_grid) {
  v_columns_all = colnames(df_grid)
  v_columns_CNT = grep("CNT_", v_columns_all, value = TRUE)
  df_grid$total_accidents <- rowSums(df_grid[, v_columns_CNT])
  return(df_grid)
}

grid_CNT <- f_create_aggregate_accidents_to_total(grid_CNT)



##########################################################################
#  Verify whether a given dataframe grid can run through the optimizer   #
##########################################################################
f_verify_valid_table_for_optimization <- function(df_grid) {
  if(!inherits(data.frame(), "data.frame")) {
    stop("Grid table is not a data frame!")
  }
  
  cols <- colnames(df_grid)
  types <- sapply(df_grid, class)
  
  df_cols_and_types <<- data.frame(cols, types)
  
  expected_cols_and_types <- cbind(c("x", "numeric"), c("y", "numeric"), c("eligible", "logical"), c("inhabitants", "numeric"), c("total_accidents", "numeric"))
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
  return(sum(checked == 5))
}

f_verify_valid_table_for_optimization(grid_CNT)


##########################################################################
# Precompute the cost of deliver any accident to any possible hospital   #
##########################################################################


# Generate Total Transport Cost Matrix
# rows = hospital cells, columns = all accident cells.
# For each cell in the grid, calculate the total cost of transporting 
# df_grid: pass a dataframe which contains the following columns: x, y, hospital_feasible. (grid_CNT is guuuud).
f_generate_matrix_transport_cost <- function(df_grid, cost_per_mile) {
  ## Make local matrix of the x,y coords of the grid
  m_grid <- cbind(df_grid$x, df_grid$y)
  
  ## Calculate distance grid
  ## BUG: this uses the eucledian distance, and isn't actually accurate with the curverature of earth
  ## but then again, for a radius of 100 miles this likely doesn't matter.
  ## A better but very computationally expensive "improvement" would be to use haversin.
  m_distances_between_all_regions <- as.matrix(dist(m_grid, diag = TRUE, upper = TRUE))
  
  ## Reduce it to a matrix to only contains the distances to hospitals
  #TODO: DO NOT USE $X < 700 BUT RATHER WHETHER THE CELL IN THE GRID IS ELIGBLE TO PUT A HOSPITAL!!!
  cells_in_grid_capable_of_hospital = as.integer(rownames(df_grid[df_grid$X < 700,]))
  # This is a horrible constant that approximates the radian distance to kilometers
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


#points <- matrix(1:8, nrow = 4, ncol = 2)
m_transport_cost_to_hospital_per_accident <- f_generate_matrix_transport_cost(grid_CNT, cost_per_mile=10)

# We must now calculate the total transport cost.
# We turn the vector of total amount of accidents into a diagonal matrix
# and then multiply the matrix of transport cost per accident with the diagonal matrix of total accidents.
# This calculate the total cost of a cell with accidents to ALL hospitals!
m_total_transport_cost_for_all_accidents <- m_transport_cost_to_hospital_per_accident %*% diag(grid_CNT$total_accidents)

# Calculate the actual final transport costs, given a matrix that assigns
# Accident cell i to Hospital cell j.
# m_assignment must be a matrix of 1 & 0's, and have the same size as m_transport.
f_calculate_total_transport_cost <- function(m_transport, m_assignment) {
  if (!identical(dim(m_transport), dim(m_assignment))){
    stop("Assignment matrix and transport matrix must be of equal length!")
  } 
  
  if (!identical(as.vector(m_assignment),as.numeric(as.logical(m_assignment)))) {
    stop("Assignment matrix must only contain binary values!")
  }
  # Applies the m_assignment as a "mask" to the transport matrix
  # Which means only the transport costs of an accident cell to the assigned hospital cell are retrieved.
  v_costs = m_transport[as.logical(m_assignment)==1]
  return(v_costs)
}


f_optimal_assignment <- function(v_hospital_assignment, m_transport_cost_per_accident) {
  # Build a mask, of all enabled hospitals for each accident.
  # If a cell of a grid is not selected to build a hospital at
  # it will be taken out of the possible places to go to.
  m_assignment_mask <- matrix( v_hospital_assignment, length(v_hospital_assignment) , ncol(m_transport_cost_per_accident))
  
  # To eliminate non-existing hospitals from being assigned, set a insane large price to get there.
  m_transport_cost_per_accident[as.logical(m_assignment_mask)==0] = 10000000
  
  assigner <- function(v_accident_column) {
    # Empty assignment
    v_assignment = rep(0, length(v_accident_column))
    # Find location i where the hospital cost is minimum, select the first hospital we find available.
    # In case multiple hospitals have the same cost.
    minimum <- c(min(v_accident_column))
    location_i <- which(v_accident_column == minimum, arr.ind = TRUE)
    # For this accident column, assign it to location i
    v_assignment[location_i[1]] = 1
    return(v_assignment)
  }
  
  m_assignement <- apply(m_transport_cost_per_accident, 2, assigner)
  
  # Re-apply the mask to be sure that the insane cost wasn't selected
  m_assignement[as.logical(m_assignment_mask)==0] = 0
  return(m_assignement)
}


##########################################################################
##                          Cost Calculation                            ##
##########################################################################

# Calculates the individual cost given that we build our hospitals at the locations indicated by v_hospital_assignment

# v_hospital_assignment: a logical vector (0 or 1's), indicating whether a hospital should be built on a cell.
# Note: to reduce the search space, the length of this vector is not all possible cells of the grid.
# But only the cells where a hospital can be built upon (rows where grid_CNT$Eligible == TRUE)
f_calculate_individual_cost <- function(v_hospital_assignment) {
  # Total amount of hospitals assigned
  individual_investment_cost = 50000000 * v_hospital_assignment
  
  # TODO: Total operational cost
  individual_operational_cost <- (v_hospital_assignment)
  
  # Total transport cost
  ## Optimize the assignment of accidents to hospitals
  m_assignment <- f_optimal_assignment(v_hospital_assignment, m_transport_cost_to_hospital_per_accident)
  ## Use the optimal assignment as a mask on the total transport cost matrix to only get the transport costs that occur.
  individual_transport_cost = f_calculate_total_transport_cost(m_total_transport_cost_for_all_accidents, m_assignment)

  # Bind all the vectors together into a matrix, 
  # each row represent a hospital, 
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
##                          Genetic Algorithm                           ##
##########################################################################

# v_hospital_assignment: a logical vector (0 or 1's), indicating whether a hospital should be built on a cell.
# Note: to reduce the search space, the length of this vector is not all possible cells of the grid.
# But only the cells where a hospital can be built upon (rows where grid_CNT$Eligible == TRUE)
f_fitness <- function(v_hospital_assignment) {
  return(f_calculate_total_cost(v_hospital_assignment))
  
}
GA <- ga("binary", fitness = f_fitness, maxiter = 1000, run = 200, seed = 123, nBits = 699)
summary(GA)


##########################################################################
##                            Post Processing                           ##
##########################################################################

f_add_optimal_solution_to_grid <- function(v_optimal_hospital_assignment, df_grid) {
  df_grid$build_hospital = FALSE
  #TODO: DO NOT USE $X < 700 BUT RATHER WHETHER THE CELL IN THE GRID IS ELIGBLE TO PUT A HOSPITAL!!!
  df_grid[df_grid$X < 700,]$build_hospital = as.logical(v_optimal_hospital_assignment)
  return(df_grid)
}

grid_CNT <- f_add_optimal_solution_to_grid(,df_grid)