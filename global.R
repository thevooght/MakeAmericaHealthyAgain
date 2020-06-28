if (!exists("optimal_grid_CNT")) {
  load(file = "data/optimal_grid_CNT.rds")
}

if (!exists("accidents_agg")) {
  accidents_agg <- read.csv(file = 'data/basetable_agg.csv', 
                        header = TRUE,
                        sep = ',')
}