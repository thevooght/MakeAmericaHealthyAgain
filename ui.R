library(leaflet)

# Choices for drop-downs
vars <- c(
  "State, City, 1" = "superzip",
  "State, City, 2" = "superzip1",
  "State, City, 3" = "superzip2",
  "State, City, 4" = "superzip3"
)


navbarPage("Make America Healthy Again", id="nav",

  tabPanel("Accident explorer",
    div(class="outer",
                        
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),
                        
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("heatmap", width="100%", height="100%"),
                        
      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 400, height = "auto",
        plotOutput("barchart"))
      )
  ),
           
  tabPanel("Hospital explorer",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%")
    )
  ),
  tabPanel("Analysis",
            fluidRow(
              column(4,
                     numericInput("investmentCost", "Investment cost (million $)", min=0, value=50)
              ),
              column(4,
                     numericInput("operationalCost", "Operational cost per inhabitant ($)", min=0, value=5000)
              ),
              column(4,
                     numericInput("transportCost", "Transport cost per mile ($)", min=0, value=10)
              ),
              column(4,
                     numericInput("maximumMiles", "Maximum distance to accidents (miles)", min=0, value=100)
              ),
              column(4,
                     numericInput("accidentsPercentage", "Change amount of accidents (%)", value=0)
              ),
              column(4,
                     actionButton("buttonSensitivity", "Calculate changes")
              )
           ),
           fluidRow(
             column(4,
                    plotOutput("barchart_total_cost")
             ),
             column(4,
                      plotOutput("barchart_investment_cost")
             ),
             column(4,
                    plotOutput("barchart_operational_cost")
             ),
             column(4,
                    plotOutput("barchart_transport_cost")
             ),
             column(4,
                    plotOutput("piechart_cost")
             ),
             column(4,
                    plotOutput("piechart_coverage")
             )
           )
  ),
  conditionalPanel("false", icon("crosshair"))
)
