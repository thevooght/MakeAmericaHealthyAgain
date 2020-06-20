f_cost_analysis <- function(forecasts, hospitals, population_per_city, investment_cost, operational_cost, years_to_operate, population_growth, transport_cost, cut_off_distance){ 
  
  total_investment_cost = f_investment_cost_calculation(hospitals, investment_cost)

  total_operational_cost = f_operational_cost_calculation(hospitals, population_per_city, operational_cost, years_to_operate, population_growth)

  total_transport_cost = f_investment_cost_calculation(hospitals, investment_cost)
  
  total_cost = total_investment_cost + total_operational_cost + total_transport_cost
  return(total_cost, total_investment_cost, total_operational_cost, total_transport_cost)
}

f_investment_cost_calculation <- function(hospitals, investment_cost) {
  ## Investment cost
  ## = Amount of hospitals * invesment_cost
  return(0)
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
  return(0)
}

f_transport_cost_calculation <- function(forecasts, hospitals, transport_cost, cut_off_distance) {
  ## Transport cost
  ## For each forecasted accident, find the closest hospital 
  ## if it is larger that the cut_off_distance then we skip it as nobody is going there :(
  ## = sum (transport_cost * distance_from_hospital_to_accident)
  
  return(0)
}