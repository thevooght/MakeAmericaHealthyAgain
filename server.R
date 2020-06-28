library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
require(tcltk)

source("5_ga_optimizer.R")

msgBox <- tkmessageBox(title = "Setup", message = "Setup calculating transport matrix may take a few minutes!", icon = "info", type = "ok")
f_setup(optimal_grid_CNT, 50000000, 5000 * 20, 10, 0.98)

# Display numbers as a whole
#options("scipen"=100, "digits"=4)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
#accidentSampled <- accidents[sample.int(nrow(accidents), 10000),]


f_setup_hospital_map <- function(input, output, session) {
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

f_plot_all_analysis_charts <- function(output) {
  
  output$barchart_total_cost <- renderPlot({
    df_merged <- as.data.frame(rbind(c("Before", sum(optimal_grid_CNT$total_cost)), c("After", sum(adjusted_grid_CNT$total_cost))))
    colnames(df_merged) <- c("Result", "Cost")
    
    r <- ggplot(data=df_merged, aes(x=Result, y=Cost)) +
      geom_bar(stat="identity") +
      ggtitle("Total Cost")
    return(r)
    
  })
  
  output$barchart_investment_cost <- renderPlot({
    df_merged <- as.data.frame(rbind(c("After", sum(adjusted_grid_CNT$investment_cost)), c("Before", sum(optimal_grid_CNT$investment_cost))))
    colnames(df_merged) <- c("Result", "Cost")
    
    r <- ggplot(data=df_merged, aes(x=Result, y=Cost)) +
      geom_bar(stat="identity") +
      ggtitle("Investment Cost")
    return(r)
    
  })
  
  output$barchart_operational_cost <- renderPlot({
    df_merged <- as.data.frame(rbind(c("Before", sum(optimal_grid_CNT$operational_cost)), c("After", sum(adjusted_grid_CNT$operational_cost))))
    colnames(df_merged) <- c("Result", "Cost")
    
    r <- ggplot(data=df_merged, aes(x=Result, y=Cost)) +
      geom_bar(stat="identity") +
      ggtitle("Operational Cost")
    return(r)
    
  })
  
  output$barchart_transport_cost <- renderPlot({
    df_merged <- as.data.frame(rbind(c("Before", sum(optimal_grid_CNT$transport_cost)), c("After", sum(adjusted_grid_CNT$transport_cost))))
    colnames(df_merged) <- c("Result", "Cost")
    
    r <- ggplot(data=df_merged, aes(x=Result, y=Cost)) +
      geom_bar(stat="identity") +
      ggtitle("Transport Cost")
    return(r)
    
  })
  
  output$piechart_cost <- renderPlot({
    total <- sum(adjusted_grid_CNT$total_cost)
    invest <- (sum(adjusted_grid_CNT$investment_cost) / total) * 100
    operational <- (sum(adjusted_grid_CNT$operational_cost) / total) * 100
    transport <- (sum(adjusted_grid_CNT$transport_cost) / total) * 100
    df_merged <- as.data.frame(rbind(c("Investment", invest),c("Operational", operational), c("Transport", transport)))
    colnames(df_merged) <- c("Type", "Cost")
    
    print(df_merged)
    
    
    r <- ggplot(df_merged, aes(x="", y=Cost, fill=Type)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) +
      ggtitle("Pie chart Cost")
    return(r)
    
  })
  
  output$piechart_coverage<- renderPlot({
    coverage <- f_check_coverage_of_solution(adjusted_grid_CNT[adjusted_grid_CNT$eligible_hospital,]$build_hospital)
    df_merged <- as.data.frame(rbind(c("Covered", coverage), c("Missed", 1 - coverage)))
    colnames(df_merged) <- c("Type", "Coverage")
    
    print(df_merged)
    
    
    r <- ggplot(df_merged, aes(x="", y=Coverage, fill=Type)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) +
      ggtitle("Covered vs missing")
    return(r)
    
  })
}




function(input, output, session) {
  hospital_map_made <<- FALSE
  f_plot_all_analysis_charts(output)
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
  
  leafletProxy("heatmap", data = accidents_agg) %>%
      #clearShapes() %>%
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
  print(input)
  observe({
    if (req(input$nav) == "Hospital explorer"){
      if (!hospital_map_made) {
        f_setup_hospital_map(input, output, session)
        hospital_map_made <<- TRUE
      }
    }
  })

  ##########
  observeEvent(input$buttonSensitivity, {
    adjusted_grid_CNT <<- optimal_grid_CNT
    bool_recalc_transport_matrix = (cte_transport_cost_per_mile != input$transportCost)

    
    cte_investment_cost_per_hospital <<- input$investmentCost * 1000000 # turn to millions
    cte_operational_cost_per_hospital <<- input$operationalCost * 20 # years
    cte_transport_cost_per_mile <<- input$transportCost
    
    cte_cutoff_transport_cost <<- cte_transport_cost_per_mile * input$maximumMiles
    adjusted_grid_CNT$total_accidents <<- (1 + input$accidentsPercentage / 100 ) * optimal_grid_CNT$total_accidents
    new_total_accidents <<- sum(adjusted_grid_CNT$total_accidents)
    bool_recalc_transport_matrix = bool_recalc_transport_matrix || (cte_sum_total_accidents != new_total_accidents)
    cte_sum_total_accidents <<- new_total_accidents
    
    v_hospital_assignment <- adjusted_grid_CNT[adjusted_grid_CNT$eligible_hospital,]$build_hospital
    adjusted_grid_CNT <<- f_build_optimal_grid(v_hospital_assignment, adjusted_grid_CNT)
    
    if (bool_recalc_transport_matrix) {
      msgBox <- tkmessageBox(title = "Recalculating", message = "Recalculating transport matrix may take a few minutes!", icon = "info", type = "ok")
      f_setup(adjusted_grid_CNT, cte_investment_cost_per_hospital, cte_operational_cost_per_hospital, cte_transport_cost_per_mile, 0.98)
      msgBox <- tkmessageBox(title = "Recalculating", message = "Recalculating done!", icon = "info", type = "ok")
    }
    
    f_plot_all_analysis_charts(output)
    
  })

}

