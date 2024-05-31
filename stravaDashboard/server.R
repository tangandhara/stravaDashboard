#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
source("leaflet_map.R")
#source("parkrun_results.R")

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Reactive expression to filter acts based on slider inputs
  filtered_acts <- reactive({
    acts %>%
      filter(total_km >= input$distance_sliderInput[1],
             total_km <= input$distance_sliderInput[2],
             total_elevation_gain >= input$elevation_sliderInput[1],
             total_elevation_gain <= input$elevation_sliderInput[2])
  })
  
  # Reactive expression to filter run_data based on filtered acts
  filtered_run_data <- reactive({
    run_data %>%
      filter(id %in% filtered_acts()$id)
  })
  

  
  output$strava_map <- renderLeaflet({
    leaflet_map(filtered_acts(), filtered_run_data())
  })
}