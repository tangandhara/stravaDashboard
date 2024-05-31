#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(googleway)
library(shinydashboard)

# Define UI for application that draws a histogram
dashboardPage(

    # Application title
  dashboardHeader("10 years of parkrun"),

    # Sidebar with a slider input for number of bins
  dashboardSidebar(
    dashboardBody(
          includeMarkdown("map_info.md"),
          #valueBoxOutput(outputId = "total_parkruns"),
          #valueBoxOutput(outputId = "total_filtered_parkruns"),        
          sliderInput(inputId = "distance_sliderInput", label = "Select a distance (km) range:",
                      min = min(acts$total_km), max = max(acts$total_km),
                      value = c(min(acts$total_km), max(acts$total_km))),
          sliderInput(inputId = "elevation_sliderInput", label = "Select a range of elevation gain (m):",
                      min = min(acts$total_elevation_gain), max = max(acts$total_elevation_gain),
                      value = c(min(acts$total_elevation_gain), max(acts$total_elevation_gain))),
),
        # Show a plot of the generated distribution
        mainPanel(
          leafletOutput(outputId = "strava_map") 
        )
    )
)
