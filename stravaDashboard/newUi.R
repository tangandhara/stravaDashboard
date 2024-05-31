
ui <- dashboardPage(
  dashboardHeader(title = strong("10 years of parkrun")),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Overview",
        tabName = "demo_tab",
        icon = icon("person-running")
      ),
      menuItem("Welcome", tabName = "demo_tab1", icon = icon("door-open"))
    ),
    menuItem("Data", tabName = "demo_tab2", icon = icon("database"))
  ),
  
  dashboardBody(fluidRow(
    box(
      width = 10,
      title = tagList(icon("location-dot"), strong("Introduction")),
      tabPanel(
        "Map summary",
        "The parkrun activities can be filtered by distance and elevation gain. These may seem pointless filters considering parkrun is a 5km time trial but, especially for many of my earlier runs at Preston Park, parkruns were sandwiched in with a longer run and were recorded as a single activity for Strava kudos. Clicking on the markers will display an activity name, distance, and date.",
        br(),
        br(),
        "Note: There's approximately 40 runs not shown on the map from around 2014-15 because they were recorded in RunKeeper rather than Strava and have since disappeared into the ether. Another 10 or so have not been picked up due to the filtering and cross-join of the Strava and parkrun datasets."
      )
    ),
    
    column(
      width = 5,
      # Value Box 1
      valueBoxOutput(outputId = "box_1", width = 3),
      
      # Value Box 2
      valueBoxOutput(outputId = "box_2", width = 3),
      
      # Value Box 3
      valueBoxOutput(outputId = "box_3", width = 3),
      
      # Value Box 4
      valueBoxOutput(outputId = "box_4", width = 3)
    ),
    br(),
    column(
      width = 5,
      title = tagList(icon("compass"), strong("Controls")),
      sliderInput(
        inputId = "distance_sliderInput",
        label = "Select a distance range (km):",
        min = min(acts$total_km),
        max = max(acts$total_km),
        value = c(min(acts$total_km), max(acts$total_km))
      ),
      sliderInput(
        inputId = "elevation_sliderInput",
        label = "Select elevation gain range (m):",
        min = min(acts$total_elevation_gain),
        max = max(acts$total_elevation_gain),
        value = c(
          min(acts$total_elevation_gain),
          max(acts$total_elevation_gain)
        )
      )
    )
  ), br(), fluidRow(
    box(
      width = 10,
      title = tagList(icon("route"), strong("Heatmap")),
      leafletOutput(outputId = "strava_map")
    )
  ))
)


server <- function(input, output) {
  output$box_1 <- shinydashboard::renderValueBox({
    valueBox(5, "Number of all-time parkruns", color = "green")
  })
  
  # Box 2
  output$box_2 <- renderValueBox({
    valueBox(10, "Parkruns shown on map", color = "blue")
  })
  
  # Box 3
  output$box_3 <- renderValueBox({
    valueBox(15, "Total distance on map", color = "purple")
  })
  
  # Box 4
  output$box_4 <- renderValueBox({
    valueBox(20, "Total elevation on map", color = "orange")
  })
  
  # Reactive expression to filter acts based on slider inputs
  filtered_acts <- reactive({
    acts %>%
      filter(
        total_km >= input$distance_sliderInput[1],
        total_km <= input$distance_sliderInput[2],
        total_elevation_gain >= input$elevation_sliderInput[1],
        total_elevation_gain <= input$elevation_sliderInput[2]
      )
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

shinyApp(ui, server)
