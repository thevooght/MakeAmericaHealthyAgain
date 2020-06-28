##########################################################################################################
#                                      MAKE AMERICA HEALTHY AGAIN                                        #
##########################################################################################################
# 6. MANAGERIAL INSIGHTS                                                                                 #
##########################################################################################################
# Group 17                                                                                               #
# Regis Demolie, Cedric Devooght, Nathan De Wilde, Florian Mathieu, Jef Van Hulle                        #
##########################################################################################################

rm(list = ls())
dir <- 'C:/! Project Prescriptive'
#dir <- paste0(getwd(), "/data")
setwd(dir = dir)
getwd()


##### LOAD DATA
##########################################################################################################

avg_cities_inhabitants <- read.csv(file = 'data/avg_inhabitants_cities.csv', 
                          header = TRUE,
                          sep = ',')
#str(avg_cities_inhabitants)
#avg_cities_inhabitants$X <- NULL

regions_cities <- read.csv(file = 'data/cities_2040.csv', 
                                  header = TRUE,
                                  sep = ',')
#str(regions_cities)
#regions_cities$X <- NULL

forecasted_accidents_wCity <- read.csv(file = 'data/forecasted_accidents_wCity.csv', 
                                       header = TRUE,
                                       sep = ',')
#str(forecasted_accidents_wCity)
#forecasted_accidents_wCity$X <- NULL

city_per_region <- read.csv(file = 'data/city_per_region.csv', 
                                       header = TRUE,
                                       sep = ',')
#str(city_per_region)
#city_per_region$X <- NULL
city_per_region$City <- as.character(city_per_region$City)
city_per_region$State <- as.character(city_per_region$State)


##### PLOTTING
##########################################################################################################

f_MapGrowth <- function(percentage=0.1, type='top', forecasted_accident_wCity, city_per_region, regions_cities, color="grey", size=5){
  
  # Required packages
  if(!require('rvest')) { install.packages('rvest', quietly = TRUE) }; require('rvest', quietly = TRUE)
  if(!require('tidyverse')) { install.packages('tidyverse', quietly = TRUE) }; require('tidyverse', quietly = TRUE)
  if(!require('stringr')) { install.packages('stringr', quietly = TRUE) }; require('stringr', quietly = TRUE)
  if(!require('ggmap')) { install.packages('ggmap', quietly = TRUE) }; require('ggmap', quietly = TRUE)

  library(rvest)
  library(tidyverse)
  library(stringr)
  library(ggmap)

  # Create insights
  forecasts_bigCities <- forecasted_accidents_wCity[forecasted_accidents_wCity$inCity==1,]
  forecasts_bigCities$Region <- as.integer(forecasts_bigCities$Region)
  forecasts_region <- merge(regions_cities[,c("City","forecast_2040")], 
                            city_per_region[,c("City","Region")], 
                            by = "City")
  insight <- merge(forecasts_region[,c("City","Region","forecast_2040")], 
                   forecasts_bigCities[,c("Region","State","nbr_inhabitants","x","y")], 
                   by = "Region")
  rm(forecasts_bigCities)
  insight$growth <- ((insight$nbr_inhabitants-insight$forecast_2040)/insight$forecast_2040)
  insight <- insight[order(-insight$growth),]
  insight <- insight[!duplicated(insight[,c('City','State')]),]
  # Some adjustments due to errors
  #insight <- insight[insight$City!='Bellingham',]
  insight[which(insight$Region == 27995),]$State <- 'IN'
  

  if(type=='Top'){
    indexTop <- floor(percentage*length(insight$growth))
    data <- insight[1:indexTop,]
    growthordecline <- ' growing '
  } 
  else {
    indexBottom <- floor((1-percentage)*length(insight$growth))
    data <- insight[indexBottom:nrow(insight),]
    data$growth <- -data$growth
    growthordecline <- ' declining '
  }
  
  # Determine breaks
  breaks <- c()
  breaks[1] <- quantile(data$growth)[1]
  breaks[2] <- (quantile(data$growth)[2] + quantile(data$growth)[3]) / 2
  breaks[3] <- quantile(data$growth)[4]
  
  attach(data)
  # Get USA map
  graphics.off()
  map.states <- map_data("state")
  ggplot(label=City) +
    geom_polygon(data = map.states, aes(x = long, y = lat, group = group)) +
    geom_point(data = data, aes(x = x, y = y, size = growth, color = State)) + 
    geom_text(aes(label=ifelse(growth > breaks[3], as.character(City), ''), x=x, y=y), hjust=0, vjust=0, color=color, size=size)+
    scale_size_continuous(breaks = breaks, labels = scales::percent_format()) +
    labs(color = "State",
         size = paste("Mean percentage", growthordecline, "growth (2040-2060)"),
         title = paste(type, percentage*100, "% fastest", growthordecline , "cities"),
         subtitle = "Based on population") + 
    theme(text = element_text(colour = "#444444", family = "Gill Sans")
          #,panel.background = element_blank()
          ,axis.title = element_blank()
          ,axis.ticks = element_blank()
          ,axis.text = element_blank()
          ,plot.title = element_text(size = 28)
          ,plot.subtitle = element_text(size = 12)
          ,legend.key = element_rect(fill = "white")
          ,panel.background = element_rect(fill = "transparent"), 
          #,plot.background = element_rect(fill = "transparent", color = NA),
          #,panel.grid.major = element_blank(), 
          #,panel.grid.minor = element_blank(), 
          #,legend.background = element_rect(fill = "transparent"), 
          #,legend.box.background = element_rect(fill = "transparent")
          ) 
}

f_MapGrowth(percentage=0.1, type='Bottom', forecasted_accident_wCity, city_per_region, regions_cities, color="white", size=5)
ggsave(plot, filename = "Bottom10.png", bg = "transparent", height = 8, width = 16)
