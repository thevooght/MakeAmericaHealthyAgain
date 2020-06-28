if (!exists("optimal_grid_CNT")) {
  load(file = "data/optimal_grid_CNT.rds")
  adjusted_grid_CNT <- optimal_grid_CNT
}

if (!exists("accidents_agg")) {
  accidents_agg <- read.csv(file = 'data/basetable_agg.csv', 
                        header = TRUE,
                        sep = ',')
}