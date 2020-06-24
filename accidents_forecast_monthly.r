### FUNCTIONS
######################################################################################################

f_accidents_basetable_region_month <- function(res, futac){
  
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
  full_raster <- rasterize(futac_coords, accident_counts, fun="count", background=0)
  
  # Plot map
  #plot(log10(accident_counts), col=brewer.pal(9,"BuPu"), sub="log10 Number of Accidents")
  
  # Create table to use in a time series
  table <- data.frame(table(cellFromXY(accident_counts, futac_pts))) 
  #should have 63 |-65--128| times 25 |50.5-25.5| times 100 (0.1 is 1/100 of 1) squared degrees or 157500
  head(table) # note that several squares do not have values for several reasons: no accidents due to no streets or located in sea
  
  # Adding left out squares with value zero
  grid_count <- data.frame(rasterToPoints(accident_counts, spatial = F))
  grid_count$Var1 <- table$Var1
  
  grid_full_count <- data.frame(rasterToPoints(full_raster, spatial = F))
  grid_full_count$Var1 <- 1:nrow(grid_full_count)
  
  
  ##############################################################
  ####### Creating table of accidents per area per month #######
  ##############################################################
  
  # Create list with year - month
  val <- c()
  for (i in 2036:2039){
    val <- c(val, paste(i,'-',c(1:12)))
  }
  
  grid_CNT <- grid_count[,-3]
  names(grid_CNT)[3] <- "Region"
  
  # Create date object
  futac$date <- format(as.POSIXct(futac$time, format='%Y-%m-%d %H:%M:%S'), format='%m/%d/%Y')
  futac$date <- as.Date(futac$date, format='%m/%d/%Y')
  
  futac_divided <- futac
  futac$year <- format(as.Date(futac$date), "%Y")
  futac$month <- format(as.Date(futac$date), "%m")
  futac$month <- str_remove(futac$month, "^0+")
  futac$monthyear <- paste(futac$year,'-',futac$month)
  
  for (i in val){
    df_new <- subset(futac[,c("Start_Lat", "Start_Lng", "monthyear")], monthyear == i)
    
    #df_new might be empty, because sometimes there were no accidents in the corresponding val (year-month)
    if (nrow(df_new) != 0){
      
      df_new$area_ha <- 0.000004
      df_new_coords <- cbind(df_new$Start_Lng, df_new$Start_Lat)
      df_new_pts <- SpatialPointsDataFrame(coords=df_new_coords, data=data.frame(df_new$area_ha))
      names(df_new_pts) <- "area_ha"
      
      # create (empty) rasters
      cell_size <- 0.25 #500m x 500m
      ncols <- ((lon_max - lon_min)/cell_size)+1
      nrows <- ((lat_max - lat_min)/cell_size)+1 
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
      
      grid_count[1] <- grid_count[2] <- NULL
      
      # Merge the new grid_count with the already existing grid_CNT
      # Purpose is adding counts per region of the new month (month equal to val)
      grid_CNT <- merge(grid_CNT, grid_count %>% dplyr::select('Var1', 'layer'), by.x = 'Region', by.y = 'Var1', all.x=TRUE )
      names(grid_CNT)[names(grid_CNT) == "layer"] <- paste('CNT_',i)
      grid_CNT$Var1 <- NULL
      
      # Indicate zero value where there were no accidents (where the merge indicated NA)
      grid_CNT[is.na(grid_CNT)] <- 0
    }
    
    else{
      grid_CNT$newcol <- 0
      names(grid_CNT)[names(grid_CNT) == "newcol"] <- paste('CNT_',i)
    }
  }
  
  return(list(grid_CNT,grid_full_count))
}

f_forecast_region_month <- function(historical_accidents) {
  
  ##############################################################
  ##### Create time series per region for the next months ######
  ##############################################################
  
  # Remove colums before the third month of 2036
  series_df <- historical_accidents[,-(1:5)]
  
  # Library to transpose matrix
  library(data.table)
  
  # Create names for forecasting
  new_vals <- c()
  for (i in 2040:2060){
    new_vals <- c(new_vals,paste(i,'-',c(1:12)))
  }
  
  # Create empty dataframe
  forecasted_df <- data.frame()
  
  # Loop over each row
  for (i in 1:nrow(series_df)){
    trans_series <- data.frame(t(series_df[i,]))
    ts_month <- ts(data = trans_series, start = c(2036,3), end = c(2039,12), frequency = 12)
    
    # Forecast
    nn_model  <- nnetar(ts_month, lamdba=0) # lamdba=0 to ensure only positive forecasts
    fc_nn <- forecast(nn_model, h=12*21)
    # transform monthly output of neural net
    fc_nn_transformed <- gather(data.frame(fc_nn), key = "month", value = "Point.Forecast")
    fc_nn_transformed$Point.Forecast <- as.numeric(fc_nn_transformed$Point.Forecast)
    
    # Create dataframe
    forecasted_df <- rbind(forecasted_df, data.frame(fc_nn_transformed)[,2])
    
  }
  
  names(forecasted_df) <- new_vals
  
  # Delete the year 2040 because that's when hospitals will be build
  forecasted_df <- forecasted_df[,-c(1:12)]
  
  # Add information about the areas to the forecasted_df
  forecasted_df$Region <- historical_accidents$Region
  forecasted_df$x <- historical_accidents$x
  forecasted_df$y <- historical_accidents$y
  
  return(forecasted_df)
}


### CREATE DATASET
######################################################################################################

futac <- read.csv(file = "futureAccidentDATA.csv",
                  header = TRUE,
                  sep = ',')

output <- f_accidents_basetable_region_month(res = 0.15, futac = futac)
grid_CNT_monthly <- output[[1]]
full_grid_CNT_monthly <- output[[2]]

# Save dataset
write.csv(grid_CNT_monthly, file = "grid_CNT_monthly.csv", row.names = TRUE)


### FORECAST
######################################################################################################

grid_CNT_monthly <- read.csv(file = "grid_CNT_monthly.csv",
                     header = TRUE,
                     sep = ',')

forecasted_accidents_monthly <- f_forecast_region_month(historical_accidents = grid_CNT_monthly)

# Write to CSV
write.csv(forecasted_accidents_monthly, "forecasted_accidents_monthly", row.names = TRUE)


### ADDING NON-ACCIDENT REGIONS
######################################################################################################

forecasted_accidents_monthly <- read.csv(file = 'forecasted_accidents_monthly', 
                                 header = TRUE,
                                 sep = ',')

f_add_nonaccident_regios <- function(forecasted_accidents, full_grid_CNT){
  #The reason for this seperate function is that it reduces the time
  #significantly while otherwise all rows would be looped in the forecast
  #to predict zero accidents.
  
  forecasted_accidents$x <- NULL #Location is already in full_grid_CNT
  forecasted_accidents$y <- NULL #Location is already in full_grid_CNT
  forecasted_accidents$X <- NULL #Index is redundant
  forecasted_accidents_allRegions <- merge(forecasted_accidents, full_grid_CNT[,-3], 
                                           by.x='Region', by.y='Var1', all.y=TRUE)
  
  #Indicate zero for regions where no accidents happened: assume that if there did 
  #not happen accidents between 2036 (month 3) and 2039 (end of the year), there will not
  #happen any in the upcoming years.
  forecasted_accidents_allRegions[is.na(forecasted_accidents_allRegions)] <- 0
  
  return(forecasted_accidents_allRegions)
}

forecasted_accidents_allRegions_monthly <- f_add_nonaccident_regios(forecasted_accidents_monthly, full_grid_CNT_monthly)

#columns x,y indicates latitude, longitude
#column Region indicates number of region

# Write to CSV
write.csv(forecasted_accidents_allRegions_monthly, "forecasted_accidents_allRegions_monthly.csv", row.names = TRUE)

