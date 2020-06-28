library(dplyr)

if (!exists("optimal_grid_CNT")) {
  load(file = "data/optimal_grid_CNT.rds")
}

if (!exists("accidents")) {
  accidents <- read.csv(file = 'data/basetable.csv', 
                        header = TRUE,
                        sep = ',')
}