f_cost_analysis <- function(accidents, hospitals, population_per_city, investment_cost, operational_cost, years_to_operate, population_growth, transport_cost, cut_off_distance){ 
  ## Clear out any previous values that might be calculated for each hospital.
  ## Perhaps the table is re-used and we want to make sure the calculations are clean.
  hospitals$investment_cost = 0
  hospitals$operational_cost = 0
  hospitals$transport_cost = 0
  
  # Cost calculation for each hospital and return the total for all hospitals.
  total_investment_cost = f_investment_cost_calculation(hospitals, investment_cost)
  total_operational_cost = f_operational_cost_calculation(hospitals, population_per_city, operational_cost, years_to_operate, population_growth)
  total_transport_cost = f_investment_cost_calculation(accidents, hospitals, investment_cost)
  
  total_cost = total_investment_cost + total_operational_cost + total_transport_cost
  return(total_cost, total_investment_cost, total_operational_cost, total_transport_cost)
}

f_investment_cost_calculation <- function(hospitals, investment_cost) {
  ## Investment cost
  ## = Amount of hospitals * invesment_cost
  hospitals$investment_cost = investment_cost
  return(sum(hospitals$investment_cost))
}

f_operational_cost_calculation <- function(hospitals, population_per_city, operational_cost, years_to_operate, population_growth) {
  ## Operational cost
  ## = sum (
  ##        operational_cost 
  ##        * inhabitants_of_city_of_hospital (??? double check) 
  ##        * ((1 + population_growth) * years_to_operate)
  ##        * years_to_operate
  ##   )
  ## for all hospitals
  
  ## Loop over the row of hospitals and set for each row the column value of 'operational_cost'.
  return(sum(hospitals$operational_cost))
}

f_transport_cost_calculation <- function(accidents, hospitals, transport_cost, cut_off_distance) {
  ## Transport cost
  ## For each forecasted accident, find the closest hospital 
  ## if it is larger that the cut_off_distance then we skip it as nobody is going there :(
  ## = sum (transport_cost * distance_from_hospital_to_accident)
  
  ## Loop over the row of hospitals and set for each row the column value of 'transport_cost'.
  return(sum(hospitals$transport_cost))
}