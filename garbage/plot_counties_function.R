f_plot_counties <- function() {
  
  #############################################
  ########### Plot map of counties ############
  #############################################
  
  if(!require('ggplot2')) { install.packages('ggplot2', quietly = TRUE) }; require('ggplot2', quietly = TRUE)
  if(!require('ggmap')) { install.packages('ggmap', quietly = TRUE) }; require('ggmap', quietly = TRUE)
  if(!require('maps')) { install.packages('maps', quietly = TRUE) }; require('maps', quietly = TRUE)
  if(!require('mapdata')) { install.packages('mapdata', quietly = TRUE) }; require('mapdata', quietly = TRUE)
  
  library(ggplot2)
  library(ggmap)
  library(maps)
  library(mapdata)
  
  states <- map_data("state")
  dim(states)
  ggplot(data = states) + 
    geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
    coord_fixed(1.3) +
    guides(fill=FALSE)  # do this to leave off the color legend
  
  #Counties USA map
  if(!require('usmap')) { install.packages('usmap', quietly = TRUE) }; require('usmap', quietly = TRUE)
  library(usmap)
  library(ggplot2)
  
  plot_usmap(regions = "counties") + 
    labs(title = "US Counties",
         subtitle = "This is a blank map of the counties of the United States.") + 
    theme(panel.background = element_rect(color = "black", fill = "lightblue"))
}