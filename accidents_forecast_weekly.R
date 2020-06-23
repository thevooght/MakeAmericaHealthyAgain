### FUNCTIONS
######################################################################################################

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

f_accidents_basetable_region_week <- function(res, futac){
  
  ##############################################
  ####### Spatial grid for all accidents #######
  ##############################################
  
  if(!require('raster')) { install.packages('raster', quietly = TRUE) }; require('raster', quietly = TRUE)
  if(!require('rasterVis')) { install.packages('rasterVis', quietly = TRUE) }; require('rasterVis', quietly = TRUE)
  if(!require('sp')) { install.packages('sp', quietly = TRUE) }; require('sp', quietly = TRUE)
  if(!require('stringr')) { install.packages('stringr', quietly = TRUE) }; require('stringr', quietly = TRUE)
  if(!require('dplyr')) { install.packages('dplyr', quietly = TRUE) }; require('dplyr', quietly = TRUE)
  
  library(raster)
  library(rasterVis)
  library(RColorBrewer)
  library(sp) #for SpatialPoints
  library(stringr)
  library(dplyr)
  
  # Define dimensions
  lon_min <- -128.0; lon_max <- -65.0; lat_min <- 25.5; lat_max <- 50.5
  # Define res size
  # res = 0.15 #degrees #default in function
  
  futac$area_ha <- 0.000004 #ha=1km^2 and surface of a vehicle is e.g. 4m^2 #value can be any number.
  futac_coords <- cbind(futac$Start_Lng, futac$Start_Lat)
  futac_pts <- SpatialPointsDataFrame(coords=futac_coords, data=data.frame(futac$area_ha)) #dataframe only needs to have the same nrow as futac_coords
  names(futac_pts) <- "area_ha"
  
  # Create (empty) rasters
  cell_size <- 0.25 
  ncols <- ((lon_max - lon_min)/cell_size)+1
  nrows <- ((lat_max - lat_min)/cell_size)+1 
  accident_counts <- raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, ymn=lat_min, ymx=lat_max, res=res, crs="+proj=longlat +datum=WGS84")
  accident_counts[] <- 0

  accident_area <- raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, ymn=lat_min, ymx=lat_max, res=res, crs="+proj=longlat +datum=WGS84")

  # rasterize: transfer values associated with spatial data to raster cells
  # in other words, calculate number of accidents per raster cell
  accident_counts <- rasterize(futac_coords, accident_counts, fun="count")

  # Plot map
  #plot(log10(accident_counts), col=brewer.pal(9,"BuPu"), sub="log10 Number of Accidents")
  
  # Create table to use in a time series
  table <- data.frame(table(cellFromXY(accident_counts, futac_pts))) 
  #should have 63 |-65--128| times 25 |50.5-25.5| times 100 (0.1 is 1/100 of 1) squared degrees or 157500
  head(table) # note that several squares do not have values for several reasons: no accidents due to no streets or located in sea
  
  # Adding left out squares with value zero
  # First, create new dataframe
  grid_count <- data.frame(rasterToPoints(accident_counts, spatial = F))
  grid_count$Var1 <- table$Var1
  
  
  #############################################################
  ####### Creating table of accidents per area per week #######
  #############################################################
  
  # Create list with year - week
  val <- c()
  for (i in 2036:2039){
    val <- c(val, paste(i,'-',c(1:52)))
  }
  
  grid_CNT <- grid_count[,-3]
  names(grid_CNT)[3] <- "Region"
  
  # Create date object
  futac$date <- format(as.POSIXct(futac$time, format='%Y-%m-%d %H:%M:%S'), format='%m/%d/%Y')
  futac$date <- as.Date(futac$date, format='%m/%d/%Y')
  
  futac_divided <- futac
  futac$year <- format(as.Date(futac$date), "%Y")
  futac$week <- format(as.Date(futac$date), "%W")
  futac$week <- str_remove(futac$week, "^0+")
  futac$weekyear <- paste(futac$year,'-',futac$week)
  
  for (i in val){
    df_new <- subset(futac[,c("Start_Lat", "Start_Lng", "weekyear")], weekyear == i)
    
    #df_new might be empty, because sometimes there were no accidents in the corresponding val (year-week)
    if (nrow(df_new) != 0){
      
      df_new$area_ha <- 0.000004
      df_new_coords <- cbind(df_new$Start_Lng, df_new$Start_Lat)
      df_new_pts <- SpatialPointsDataFrame(coords=df_new_coords, data=data.frame(df_new$area_ha))
      names(df_new_pts) <- "area_ha"
      
      # create (empty) rasters
      cell_size <- 0.25 #500m x 500m
      ncols <- ((lon_max - lon_min)/cell_size)+1; nrows <- ((lat_max - lat_min)/cell_size)+1 
      accident_counts <- raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, ymn=lat_min, ymx=lat_max, res=res, crs="+proj=longlat +datum=WGS84")
      accident_counts[] <- 0

      accident_area <- raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, ymn=lat_min, ymx=lat_max, res=res, crs="+proj=longlat +datum=WGS84")

      # rasterize
      accident_counts <- rasterize(df_new_coords, accident_counts, fun="count")
      
      # Create table to use in a time series
      table <- data.frame(table(cellFromXY(accident_counts, df_new_pts))) 
      
      # Adding left out squares with value zero
      grid_count <- data.frame(rasterToPoints(accident_counts, spatial = F))
      grid_count$Var1 <- table$Var1
      
      grid_count[1] <- grid_count[2] <-NULL
      
      # Merge the new grid_count with the already existing grid_CNT
      # Purpose is adding counts per region of the new week (week equal to val)
      grid_CNT <- merge(grid_CNT, grid_count %>% dplyr::select('Var1', 'layer'), by.x = 'Region', by.y = 'Var1', all.x=TRUE )
      names(grid_CNT)[names(grid_CNT) == "layer"] <- paste('CNT_',i)
      grid_CNT$Var1<-NULL
      
      # Indicate zero value where there were no accidents (where the merge indicated NA)
      grid_CNT[is.na(grid_CNT)] <- 0
    }
    
    else{
      grid_CNT$newcol <- 0
      names(grid_CNT)[names(grid_CNT) == "newcol"] <- paste('CNT_',i)
    }
  }
  
  return(grid_CNT)
}

f_forecast_region_week <- function(historical_accidents) {
  
  #############################################################
  ##### Create time series per region for the next weeks ######
  #############################################################

  # Remove colums before the seventh week of 2036
  series_df <- historical_accidents[,-(1:9)]
  #names(historical_accidents[,-(1:9)])

  # Library to transpose matrix
  library(data.table)

  # Create names for forecasting
  new_vals <- c()
  for (i in 2040:2060){
   new_vals <- c(new_vals,paste(i,'-',c(1:52)))
  }

  # Create empty dataframe
  forecasted_df <- data.frame()

  # Temporary datalist
  #datalist = list()

  # Loop over each row
  for (i in 1:nrow(series_df)){
   trans_series <- data.frame(transpose(series_df[i,]))
   names(trans_series) <- 'Freq'
   ts_week <- ts(data = trans_series,
                 start = c(2036,7), end = c(2039,52), frequency = 52)

    # Forecast
    nn_model  <- nnetar(ts_week)
    fc_nn <- forecast(nn_model, h=52*21)

    # Create dataframe
    #datalist[[i]] <- data.frame(forecast_ets)[,1]
    forecasted_df <- rbind(forecasted_df, data.frame(fc_nn)[,1])
  
  }
  
  names(forecasted_df) <- new_vals

  # Delete the year 2040 because that's when hospitals will be build
  forecasted_df <- forecasted_df[,-c(1:52)]

  # Add information about the areas to the forecasted_df
  forecasted_df$Region <- grid_CNT$Region
  forecasted_df$x <- grid_CNT$x
  forecasted_df$y <- grid_CNT$y
  
  return(forecasted_df)
}


### CREATE DATASET
######################################################################################################

futac <- read.csv(file = "futureAccidentDATA.csv",
                  header = TRUE,
                  sep = ',')

grid_CNT <- f_accidents_basetable_region_week(res = 0.15, futac = futac)

# Check
#sum(grid_CNT[1,-c(1:3)])

# Save dataset
write.csv(grid_CNT, file = "grid_CNT.csv", row.names = TRUE)


### FORECAST
######################################################################################################

grid_CNT <- read.csv(file = "grid_CNT.csv",
                       header = TRUE,
                       sep = ',')

forecasted_accidents <- f_forecast_region_week(historical_accidents = grid_CNT)

# Write to CSV
write.csv(forecasted_accidents, "forecasted_accidents.csv", row.names = TRUE)
  