library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
#accidentSampled <- accidents[sample.int(nrow(accidents), 10000),]


function(input, output, session) {

  ## Interactive Map ###########################################

  #################### ACCIDENTS ########################
  
  # Create the map with the accidents
  output$heatmap <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -90, lat = 35, zoom = 4)
  })
  
  accidents_agg <- aggregate(X~City, accidents, length)
  accidents_agg$long <- aggregate(Start_Lng~City, accidents, mean)[,2]
  accidents_agg$lat <- aggregate(Start_Lat~City, accidents, mean)[,2]
  
  leafletProxy("heatmap", data = accidents_agg) %>%
      clearShapes() %>%
      addHeatmap(lng=~long, lat=~lat, intensity=~X, radius=10, blur=20, max=200)
  
  accidents_agg_ordered <- accidents_agg[order(-accidents_agg$X),][1:15,]
  v_accidents <- accidents_agg_ordered$X
  names(v_accidents) <- accidents_agg_ordered$City

  output$barchart <- renderPlot({
    par(mar=c(4.1, max(2.1, max(nchar(names(v_accidents)))/1.8), 4.1, 2.1))
    barplot(rev(v_accidents),
            horiz = TRUE,
            main = "Top 15 cities with most accidents",
            las = 2)
  })
  
  #################### HOSPITALS ########################
  
  # Create the map with the hospitals
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  leafletProxy("map", data = optimal_grid_CNT[optimal_grid_CNT$build_hospital,]) %>%
    addCircles(~x, ~y, radius=160000,
               stroke=FALSE, fillOpacity=0.4, fillColor="#187bcd")
  
  # Build the marker icon
  hospitalIcon <- makeIcon(
    iconUrl = "img/38-hospital.png",
    iconWidth = 38, iconHeight = 38,
    iconAnchorX = 19, iconAnchorY = 19)

  # Load all the hospitals to the map
  leafletProxy("map", data = optimal_grid_CNT[optimal_grid_CNT$build_hospital==1,]) %>%
    addMarkers(~x, ~y, icon = hospitalIcon)
  
  # Show a popup at the given location
  showHospitalPopup <- function(hospital, lat, lng) {
    selectedHospital <- optimal_grid_CNT[optimal_grid_CNT$x == lng & optimal_grid_CNT$y == lat,]
    id <- rownames(optimal_grid_CNT)[optimal_grid_CNT$x == lng & optimal_grid_CNT$y == lat]
    #print(selectedHospital)
    #optimal_grid_CNT[optimal_grid_CNT$x == lng & optimal_grid_CNT$y == lat,]$build_hospital <- FALSE
    #optimal_grid_CNT <<- optimal_grid_CNT
    #print(sum(optimal_grid_CNT$build_hospital))
    addHospitalRadius(lat, lng)
    content <- as.character(tagList(
      tags$h4("ID: ", id),
      tags$strong("Location"), tags$br(),
      sprintf("Latiude: %s°", lat), tags$br(),
      sprintf("Longitude: %s°", lng), tags$br(),
      #tags$strong("General Statistics"), tags$br(),
      #sprintf("Accidents in radius: %s", 100), tags$br(),
      tags$strong("Hospital Costs"), tags$br(),
      sprintf("Total cost : %s", dollar(selectedHospital$total_cost)), tags$br(),
      sprintf("Investment cost : %s", dollar(selectedHospital$investment_cost)), tags$br(),
      sprintf("Operational cost : %s", dollar(selectedHospital$operational_cost)), tags$br(),
      sprintf("Transport cost : %s", dollar(selectedHospital$transport_cost))
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = hospital)
  }
  
  addHospitalRadius <- function(lat, lng) {
    leafletProxy("map") %>%
      addCircles(lng, lat, radius=160934, layerId="HospitalRadius",
               stroke=FALSE, fillOpacity=0.4, fillColor="#522d80")
  }
  
  clearHospitalRadius <- function(map) {
    leafletProxy("map") %>%
      removeShape(layerId="HospitalRadius")
  }
  
  # When map is clicked, show a popup with city info
  observe({
    #clearHospitalRadius()
    leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click
    if (is.null(event))
      return()
    
    isolate({
      showHospitalPopup(event$id, event$lat, event$lng)
    })
  })

}
