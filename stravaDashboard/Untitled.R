source("leaflet_map.R")
library(DT)
library(tidyverse)
library(rStrava)
library(httr)
library(lubridate)
library(leaflet)
library(leaflet.extras)
library(googleway)
library(htmlwidgets)
library(htmltools)
library(readxl)
library(shiny)
library(shinydashboard)


acts <- readRDS("strava_acts_2024-05-28.rds")
parkrun_res <- readRDS("parkrun_acts_2024-05-28.rds")
run_data <- acts |>  filter(sport_type == "Run")

event_choices <- c(
  "All" = "All",
  "Preston Park" = "Preston Park",
  "Worthing" = "Worthing",
  "Edgbaston Reservoir" = "Edgbaston Reservoir",
  "Cannon Hill" = "Cannon Hill",
  "Hove Promenade" = "Hove Promenade"
)


# Header ------------------------------------------------------------------
header <- dashboardHeader(title = strong("A decade of parkrun"))


# Body1 -------------------------------------------------------------------


body1 <- dashboardBody(tabItems(
  tabItem(tabName = "maintab", fluidRow(column(
    width = 12,
    box(
      title = strong("Welcome to my parkrun history"),
      solidHeader = TRUE,
      status = "info",
      width = NULL,
      #background = "light-blue",
      "October 2024 will be my tenth year of parkrun and what better way to celebrate this than visualise the data and see how my performance has changed over time. This page shows where I've done my parkruns and lets you see my results. Over on the data page, I've put together some charts that analyse results in a bit more detail.",
      br(),
      br(),
      "There are a couple of things to note. First, unfortunately, there's approximately 40 runs not shown on the map from around 2014-15 because they were recorded in RunKeeper rather than Strava and have since disappeared into the ether. Another 10 or so have not been picked up due to the merging of Strava and parkrun data. This means that where Strava data is being used in conjunction with parkrun results these runs will be missing but if only parkrun data is being used then they are included.",
      br(),
      br(),
      "You should also be aware that some parkruns were sandwiched in with a longer run and were recorded as a single activity so the total activity distance is greater than 5K. This complicates some of the other analysis when looking at Strava data for these activities but they're still included in the data.",
      br(),
      br(),
      "~ Tan"
      
    )
    
  ))),
  #tabitem maintab close bracket
  tabItem(
    tabName = "tab1",
    fluidRow(
      column(
        width = 3,
        # Value Box 3
        valueBoxOutput(outputId = "box_3", width = NULL),
        # Value Box 4
        valueBoxOutput(outputId = "box_4", width = NULL)
      ),
      
      column(
        width = 3,
        # Value Box 1
        valueBoxOutput(outputId = "box_1", width = NULL),
        # Value Box 2
        valueBoxOutput(outputId = "box_2", width = NULL)
        
      ),
      
      column(
        width = 3,
        box(
          title = "Select a parkrun",
          width = NULL,
          status = "info",
          solidHeader = TRUE,
          uiOutput("parkrunSelect"),
          checkboxGroupInput("parkruns", NULL, choices = event_choices, selected = "All"),
          actionButton("zoomButton", "Zoom to fit parkruns")
        )
      ),
      box(
        width = 3,
        title = "Select a year:",
        status = "info",
        solidHeader = TRUE,
        sliderInput(
          inputId = "distance_sliderInput",
          label = NULL,
          min = min(year(run_data$Date)),
          max = max(year(run_data$Date)),
          value = c(min(year(run_data$Date)), max(year(run_data$Date)))
        )
      ),
      
      box(
        width = 3,
        title = "Select a finish time:",
        status = "info",
        solidHeader = TRUE,
        sliderInput(
          inputId = "elevation_sliderInput",
          label = NULL,
          min = min(run_data$timer_period),
          max = max(run_data$timer_period),
          value = c(min(run_data$timer_period), max(run_data$timer_period)),
          
        )
      )
    ),
    
    fluidRow(column(
      width = 7,
      box(
        title = "Locations",
        status = "info",
        width = NULL,
        solidHeader = TRUE,
        leafletOutput(outputId = "strava_map", height = 700)
      )
    ), column(
      width = 5,
      box(
        title = "Results",
        solidHeader = TRUE,
        status = "info",
        width = NULL,
        DTOutput("numParkrunsTable")
      )
    )),
    
  )
))







# Sidebar -----------------------------------------------------------------

sidebar <- dashboardSidebar(sidebarMenu(
  id = "tabs",
  menuItem(
    "Welcome",
    tabName = "maintab",
    icon = icon("person-running")
  ),
  menuItem("Overview", tabName = "tab1", icon = icon("door-open")),
  menuItem("Data", tabName = "tab2", icon = icon("database"))
))




# UI ----------------------------------------------------------------------


ui <- dashboardPage(header, sidebar, body1, title = "Parkrun Dashboard", skin = "blue")




# Server ------------------------------------------------------------------


all_time_parkruns <- parkrun_res |> nrow()


server <- function(input, output, session) {
  output$box_3 <- renderValueBox({
    valueBox(all_time_parkruns,
             "Number of all-time parkruns",
             color = "orange")
  })
  
  output$box_1 <- renderValueBox({
    valueBox(10, "Parkruns shown on map", color = "blue")
  })
  
  output$box_2 <- shinydashboard::renderValueBox({
    valueBox(5, "Fastest time on map", color = "blue")
  })
  
  output$box_4 <- shinydashboard::renderValueBox({
    valueBox(5, "All-time fastest", color = "orange")
  })
  
  filtered_acts <- reactive({
    selected_events <- input$parkruns
    if (is.null(selected_events) ||  "All" %in% selected_events) {
      return(acts)
    }
    acts %>% filter(event %in% selected_events)
  })
  
  filtered_run_data <- reactive({
    run_data %>%
      filter(id %in% filtered_acts()$id)
  })
  output$strava_map <- renderLeaflet({
    leaflet_map(filtered_acts(), filtered_run_data())
  })
  
  
  observeEvent(input$zoomButton, {
    filtered_data <- filtered_acts()
    if (nrow(filtered_data) > 0) {
      bounds <- filtered_data %>%
        summarize(
          minLat = min(lat, na.rm = TRUE),
          maxLat = max(lat, na.rm = TRUE),
          minLng = min(lng, na.rm = TRUE),
          maxLng = max(lng, na.rm = TRUE)
        )
      leafletProxy("strava_map") %>%
        fitBounds(
          lng1 = bounds$minLng,
          lat1 = bounds$minLat,
          lng2 = bounds$maxLng,
          lat2 = bounds$maxLat
        ) |>
        setView(
          lng = (bounds$minLng + bounds$maxLng) / 2,
          lat = (bounds$minLat + bounds$maxLat) / 2,
          zoom = 15  # Set your desired zoom level here
        )
    }
  })
  output$numParkrunsTable <- renderDT({
    req(input$parkruns)
    filtered_run_data() |>
      mutate(Year = year(Date)) |>
      rename(
        parkrun = event,
        "Activity Name" = name,
        "Total Activity Distance" = total_km
      ) |>
      select(parkrun, Year, Time, "Total Activity Distance") |>
      arrange(Time)
  })
}



shinyApp(ui, server)
