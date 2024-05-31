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
library(plotly)
library(ggsci)
library(ggthemes)
library(ggtext)
library(gt)
library(gtExtras)



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

col_palette  <- c(
  "Preston Park" = "#3F88C5",
  "Worthing" = "#D00000",
  "Edgbaston Reservoir" = "#FFBA08",
  "Cannon Hill" = "#136F63",
  "Hove Promenade" = "#032b43"
)

my_link <- a("Sam Csik", href = "https://samanthacsik.shinyapps.io/strava_dashboard/")
my_link2 <- a("CJ Mayes", href = "https://x.com/_CJMayes")
my_link3 <- a("Github", href = "https://github.com/samanthacsik/strava-dashboard")
my_link4 <- a("{rStrava}", href = "https://github.com/fawda123/rStrava")
my_link5 <- a("Github", href = "https://github.com/tangandhara/stravaDashboard/blob/main/stravaDashboard/stravaDashboard.R")

# Header ------------------------------------------------------------------
header <- dashboardHeader(title = strong("A decade of parkrun"))

# Body1 -------------------------------------------------------------------


body1 <- dashboardBody(
  tabItems(
    tabItem(tabName = "maintab",
            fluidRow(
              column(
                width = 12,
                tags$img(class = "banner", src = "media/parkrun_wide.jpeg",
                         alt = "Runners heading towards the finish of the Worthing parkrun.",
                         width = "100%", height = "auto"),
                br(),
                br()
              )
            ),
            fluidRow(
              column(
                width = 6,
                box(
                  title = strong("Running data geeks unite!"),
                  solidHeader = TRUE,
                  status = "success",
                  width = NULL,
                  "Hi, I'm Tan! In October 2024 I'll have completed my tenth year of parkrun and what better way to celebrate this than visualise the data and see how my performance has changed over time. ",
                  br(),
                  br(),
                  "On the dashboard page, you can see where I've run and some of the data associated with the runs from Strava. It's possible to filter the data by location, finish time, and year. Please note that there's approximately 40 parkruns that do not show on the map from around 2014-15 because they were recorded in RunKeeper rather than Strava and another 10 or so have not been picked up due to the merging of Strava and parkrun data. For this reason there's a discrepancy between my all-time parkruns and those shown on that page.",
                  br(),
                  br(),
                  "The analysis page shows all parkruns across all years, and you can explore results and participation over time, performance at each event, and the distribution of results by finish time.",
                  br(),
                  br()
                )
              ),
              column(
                width = 6,
                box(
                  title = strong("parkrun is great but why this?"),
                  solidHeader = TRUE,
                  status = "success",
                  width = NULL,
                  "I've been inspired by other people's efforts to visualise their Strava data (especially ", my_link, " and ", my_link2, "). I also wanted to see what I could do with parkrun specific data to track my own progress.",
                  br(),
                  br(),
                  "I also wanted to develop my skills with Shiny in an effort to move beyond Tableau and Power BI for building dashboards. I've got a couple of ideas for other apps which I hope to build over the summer, so getting this one finished was a good warm up for those.",
                  br(),
                  br(),
                  "A massive thank you to Sam for sharing her code on ", my_link3, ", especially for getting to grips with the ", my_link4, " package, which has allowed me to make a start with this - it's been much easier to piggyback off her work than start from scratch! The code for this site is on my ",my_link5," and I hope it helps someone else too.",
                  br(),
                  br()
                )
              )
            )
    )
  ,
  #tabitem maintab close bracket
  tabItem(
    tabName = "tab1",
    fluidRow(
      column(
        width = 6,
        fluidRow(
          column(
            width = 6,
            # Value Box 3
            valueBoxOutput(outputId = "box_3", width = NULL)
          ),
          column(
            width = 6,
            # Value Box 4
            valueBoxOutput(outputId = "box_1", width = NULL)
          )
        ),
        fluidRow(
          column(
            width = 6,
            # Value Box 1
            valueBoxOutput(outputId = "box_4", width = NULL)
          ),
          column(
            width = 6,
            # Value Box 2
            valueBoxOutput(outputId = "box_2", width = NULL)
          )
        ),
        fluidRow(
          column(
            width = 6,
            box(
              title = tagList(icon("compass"), strong("Select a parkrun")),
              width = NULL,
              height = 295,
              status = "success",
              solidHeader = TRUE,
              uiOutput("parkrunSelect"),
              checkboxGroupInput("parkruns", NULL, choices = event_choices, selected = "All"),
              actionButton("zoomButton", "Zoom to selected parkrun")
            )
          ),
          column(
            width = 6,
            box(
              width = NULL,
              title = tagList(icon("calendar"), strong("Select a year:")),
              status = "success",
              solidHeader = TRUE,
              sliderInput(
                sep = "",
                inputId = "year_sliderInput",
                step = 1,
                label = NULL,
                min = min(year(run_data$Date)),
                max = max(year(run_data$Date)),
                value = c(min(year(run_data$Date)), max(year(run_data$Date)))
              )
            ),
            box(
              width = NULL,
              title = tagList(icon("clock"), strong("Select a finish time:")),
              status = "success",
              solidHeader = TRUE,
              sliderInput(
                inputId = "time_sliderInput",
                label = NULL,
                min = min(round(run_data$decimal_time)),
                max = max(round(run_data$decimal_time)),
                value = c(min(round(run_data$decimal_time)),
                          max(round(run_data$decimal_time))),
              )
            )
          )
        )
      ),
      column(
        width = 6,
        box(
          title = strong("Locations"),
          status = "success",
          width = NULL,
          solidHeader = TRUE,
          leafletOutput(outputId = "strava_map", height = 480)
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        box(
          title = strong("Results"),
          solidHeader = TRUE,
          status = "success",
          width = NULL,
          gt_output("numParkrunsTable")
        )
      )
    ),
    fluidRow()
  )
  ,
  #tabitem close overview page
  
  tabItem(tabName = "tab2", fluidRow(column(
    width = 3,
    box(
      title = tagList(icon("magnifying-glass-chart"), strong("Results analysis")),
      width = NULL,
      status = "success",
      solidHeader = TRUE,
      "This page lets you look at my results in a more detailed way and the charts are interactive so feel free to make use of the functions to explore the data. The data is presented in three tabs:",
      br(),
      br(),
      strong("Results over time"),
      "shows how my results have changed over time between October 2014 until May 2024. The gap between March 2020 and July 2021 was because of the pandemic which meant that all parkrun events were cancelled in line with government guidance at the time.",
      br(),
      br(),
      strong("Participation over time"),
      "shows the number of parkruns completed over time, including when I achieved my 100th and 200th milestones .",
      br(),
      br(),
      strong("Results by event"),
      "shows the spread of results at each event using a combination of box and scatter plots. The horizontal line in the middle of each box is my median time for that event, while the q1 and q3 times show the spread of the middle 50% of my times (in a decimal format because R doesn't like time periods). ",
      br(),
      br(),
      strong("Distribution of results"),
      "shows a breakdown of the number of occassions I finished in each time window."
    )
  ), fluidRow(
    column(
      width = 8,
      tabBox(
        id = "parkrun_tabbox",
        title = strong("Results analysis"),
        width = NULL,
        height = 700,
        tabPanel("Results over time", plotlyOutput("myplot")),
        tabPanel("Participation over time", plotlyOutput("myplot3")),
        tabPanel("Results by event", plotlyOutput("myplot2")),
        tabPanel("Distribution of results", plotlyOutput("myplot1"))
      )
    )
    
  )))
) #tabitems close) #dashboardBody close

)






# Sidebar -----------------------------------------------------------------

sidebar <- dashboardSidebar(sidebarMenu(
  id = "tabs",
  menuItem(
    "Welcome",
    tabName = "maintab",
    icon = icon("person-running")
  ),
  menuItem("Dashboard", tabName = "tab1", icon = icon("database")),
  menuItem(
    "Analysis",
    tabName = "tab2",
    icon = icon("chart-line")
  )
))




# UI ----------------------------------------------------------------------


ui <- dashboardPage(header, sidebar, body1, title = "Parkrun Dashboard", skin = "green")




# Server ------------------------------------------------------------------


all_time_parkruns <- parkrun_res |> nrow()
all_time_fastest <- parkrun_res |> arrange(Time) |> head(1) |> pull(Time)

server <- function(input, output, session) {
  
  output$box_3 <- renderValueBox({
    valueBox(all_time_parkruns,
             "Number of all-time parkruns",
             color = "blue", icon = icon("person-running"))
  })
  
  output$box_1 <- renderValueBox({
    valueBox(nrow(filtered_acts()), "Parkruns shown on map", color = "yellow", icon = icon("person-running"))
  })
  
  output$box_2 <- shinydashboard::renderValueBox({
    first_time <- table_data()$Time[1]
    valueBox(first_time, "Fastest time shown on map", color = "yellow",icon = icon("clock"))
  })
  
  output$box_4 <- shinydashboard::renderValueBox({
    valueBox(all_time_fastest, "All-time fastest parkrun", color = "blue", icon = icon("clock"))
  })
  
  filtered_acts <- reactive({
    selected_events <- input$parkruns
    if (is.null(selected_events) || "All" %in% selected_events) {
      selected_events <- unique(acts$event)
    }
    acts %>%
      filter(
        event %in% selected_events,
        year(Date) >= input$year_sliderInput[1],
        year(Date) <= input$year_sliderInput[2],
        round(decimal_time, 1) >= input$time_sliderInput[1],
        round(decimal_time, 1) <= input$time_sliderInput[2]
      )
  })
  
  
  
  filtered_run_data <- reactive({
    run_data %>%
      filter(id %in% filtered_acts()$id)
  })
  
  
  output$strava_map <- renderLeaflet({
    leaflet_map(filtered_acts(), filtered_run_data())
  })
  
  table_data <- reactive({
    filtered_run_data() |>
      mutate(Year = year(Date),
             "Average Speed (km/h)" = round(average_speed,1)) |>
      rename(
        parkrun = event,
        "Activity Name" = name,
        "Total Activity Distance (km)" = total_km,
        "Average Heartrate (bpm)" = average_heartrate,
        "Total Elevation Gain (m)" = total_elevation_gain,
        "Strava Achievments"  = achievement_count,
        "Strava Kudos" = kudos_count
      ) |>
      select(parkrun, Year, Time, "Total Activity Distance (km)", "Average Heartrate (bpm)", "Total Elevation Gain (m)" , "Average Speed (km/h)", PB, "Strava Achievments" , "Strava Kudos") |>
      arrange(Time)
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
  
  output$numParkrunsTable <- render_gt({
    req(input$parkruns)
    table_data() |>
      gt() |>
      tab_spanner(label = "Results", columns = everything()) |>
      data_color(
        columns = "Time",
        method = "factor",
        palette = "Set1"
      ) |> 
      tab_style(
        style = list(
          #cell_text(color = "black")
        ),
        locations = cells_body(columns = "Average Heartrate (bpm)")
      ) |>
      text_transform(
        locations = cells_body(columns = "Average Heartrate (bpm)"),
        fn = function(x) {
          if_else(x < 135,
                  paste0(x, " <i class='fa fa-heart' style='color: green;'></i>"),
                  if_else(x < 160,
                          paste0(x, " <i class='fa fa-heart' style='color: orange;'></i><i class='fa fa-heart' style='color: orange;'></i>"),
                          paste0(x, " <i class='fa fa-heart' style='color: red;'></i><i class='fa fa-heart' style='color: red;'></i><i class='fa fa-heart' style='color: red;'></i>")))
        }
      ) |>
      text_transform(
        locations = cells_body(columns = "PB"),
        fn = function(x) {
          if_else(x == "PB",
                  paste0(x, " <i class='fa fa-smile'></i>"),
                  x)
        }
      ) |> 
      text_transform(
        locations = cells_body(columns = "Strava Kudos"),
        fn = function(x) {
          if_else(x > 0,
                  paste0(x, " <i class='fa fa-thumbs-up' style='color: orange;'></i>"),
                  x)
        }
      )|>
      text_transform(
        locations = cells_body(columns = "Strava Achievments"),
        fn = function(x) {
          if_else(x > 0,
                  paste0(x, " <i class='fa fa-circle-check' style='color: green;'></i>"),
                  x)
        }
      ) |>
      text_transform(
        locations = cells_body(columns = "Total Elevation Gain (m)"),
        fn = function(x) {
          if_else(x > 1,
                  paste0(x, " <i class='fa fa-arrow-trend-up' style='color: blue;'></i>"),
                  x)
        }
      ) |>
      text_transform(
        locations = cells_body(columns = "Average Speed (km/h)"),
        fn = function(x) {
          if_else(x > 1,
                  paste0(x, " <i class='fa fa-gauge' style='color: #8338EC;'></i>"),
                  x)
        }
      ) |>  tab_source_note(
        source_note = md("Some **Total Activity Distances** are greater than 5K because the parkrun was part of a longer run that was recorded as a single actitivity in Strava.")
      ) |>
      tab_header(
        title = html("Combined <strong>parkrun</strong> and <strong>Strava</strong> activity data")) |> 
      opt_interactive()
  })
  
  
  
  
  
  output$myplot <- renderPlotly({
    p <- ggplot(parkrun_res, aes(
      x = as.Date(Date),
      y = round(decimal_time, 1),
      text = paste("Time:", parkrun_res$Time)
    )) +
      geom_point(aes(colour = parkrun_res$`Event `), alpha = 0.5) +
      #scale_color_manual(values = col_palette)+
      #scale_fill_manual(values = col_palette)+
      #theme_fivethirtyeight()+
      labs(
        x        = "",
        y        = "Time (mins)",
        title    = "",
        colour = "Location",
        caption = "test"
      ) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      scale_y_continuous(breaks = seq(20, max(round(
        parkrun_res$decimal_time, 1
      )), by = 1)) +
      theme(
        axis.title = element_markdown(
          margin = margin(0, 0, 10, 0),
          size   = rel(.9),
          color  = "black",
          family        = "Futura",
          face   = "bold"
        ),
        legend.title = element_markdown(
          size   = rel(.9),
          hjust  = 0.5,
          family        = "Futura",
          face   = "bold",
          color  = "black"
        ),
        legend.text = element_text(
          size   = rel(.7),
          family        = "Futura",
          face   = "bold",
          color  = "black"
        ),
        plot.title      = element_text(
          size          = rel(2.1),
          family        = "Futura",
          face          = "bold",
          color         = "black",
          lineheight    = 1.1,
          margin        = margin(t = 5, b = 1)
        ),
        plot.subtitle   = element_textbox_simple(
          size          = rel(1.1),
          family        = "Futura",
          color         = "black",
          lineheight    = 1.4,
          margin        = margin(t = 2, b = 8)
        ),
        axis.text  = element_text(size = rel(.8), family        = "Futura"),
        axis.title.x  = element_text(
          size = rel(1.3),
          family        = "Futura",
          face = "bold",
          hjust = 0.5,
          vjust = -5
        ),
        axis.title.y  = element_text(
          size = rel(1),
          family        = "Futura",
          face = "bold",
          hjust = 0.5,
          vjust = 2
        ),
        panel.grid.major = element_line(colour = "#ced4da", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")
      )
    
    p |> ggplotly(tooltip = c('Event', 'text'), height = 600)
  })
  
  
  output$myplot1 <- renderPlotly({
    p1 <- ggplot(parkrun_res, aes(x = floor(decimal_time))) +
      geom_histogram(binwidth = 1) +
      labs(
        x        = "Time (mins)",
        subtitle = "",
        y        = "Count",
        colour = "Location"
      ) +
      scale_color_manual() +
      scale_fill_manual() +
      scale_x_continuous(breaks = seq(19, 30, by = 1)) +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        axis.title = element_markdown(
          margin = margin(0, 0, 10, 0),
          size   = rel(.9),
          color  = "black",
          family        = "Futura",
          face   = "bold"
        ),
        legend.title = element_markdown(
          size   = rel(.9),
          hjust  = 0.5,
          family        = "Futura",
          face   = "bold",
          color  = "black"
        ),
        legend.text = element_text(
          size   = rel(.7),
          family        = "Futura",
          face   = "bold",
          color  = "black"
        ),
        plot.title      = element_text(
          size          = rel(2.1),
          family        = "Futura",
          face          = "bold",
          color         = "black",
          lineheight    = 1.1,
          margin        = margin(t = 5, b = 1)
        ),
        plot.subtitle   = element_textbox_simple(
          size          = rel(1.1),
          family        = "Futura",
          color         = "black",
          lineheight    = 1.4,
          margin        = margin(t = 2, b = 8)
        ),
        axis.text  = element_text(size = rel(.8), family        = "Futura"),
        axis.title.x  = element_text(
          size = rel(0.9),
          family        = "Futura",
          face = "bold",
          hjust = 0.5,
          vjust = -5
        ),
        axis.title.y  = element_text(
          size = rel(1),
          family        = "Futura",
          face = "bold",
          hjust = 0.5,
          vjust = 2
        ),
        panel.grid.major = element_line(colour = "#ced4da", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")
      )
    
    p1 |> ggplotly(tooltip = c('count'), height = 600)
    
  })
  
  output$myplot2 <- renderPlotly({
    p2 <- ggplot(parkrun_res,
                 aes(
                   x = `Event `,
                   y = round(decimal_time, 1),
                   colour = `Event `,
                   text = paste("Time:", parkrun_res$Time)
                 )) +
      geom_jitter(alpha = 0.2) +
      geom_boxplot() +
      labs(
        x        = "",
        subtitle = "",
        y        = "Time (mins)",
        colour = "Location"
      ) +
      scale_y_continuous(breaks = seq(20, max(round(
        parkrun_res$decimal_time, 1
      )), by = 1)) +
      theme(
        axis.title = element_markdown(
          margin = margin(0, 0, 10, 0),
          size   = rel(.9),
          color  = "black",
          family        = "Futura",
          face   = "bold"
        ),
        legend.title = element_markdown(
          size   = rel(.9),
          hjust  = 0.5,
          family        = "Futura",
          face   = "bold",
          color  = "black"
        ),
        legend.text = element_text(
          size   = rel(.7),
          family        = "Futura",
          face   = "bold",
          color  = "black"
        ),
        plot.title      = element_text(
          size          = rel(2.1),
          family        = "Futura",
          face          = "bold",
          color         = "black",
          lineheight    = 1.1,
          margin        = margin(t = 5, b = 1)
        ),
        plot.subtitle   = element_textbox_simple(
          size          = rel(1.1),
          family        = "Futura",
          color         = "black",
          lineheight    = 1.4,
          margin        = margin(t = 2, b = 8)
        ),
        axis.text  = element_text(size = rel(.8), family        = "Futura"),
        axis.title.x  = element_text(
          size = rel(1.3),
          family        = "Futura",
          face = "bold",
          hjust = 0.5,
          vjust = -5
        ),
        axis.title.y  = element_text(
          size = rel(1),
          family        = "Futura",
          face = "bold",
          hjust = 0.5,
          vjust = 2
        ),
        panel.grid.major = element_line(colour = "#ced4da", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")
      )
    
    p2 |> ggplotly(tooltip = c('Event', 'text'), height = 600)
    
    
  })
  
  output$myplot3 <- renderPlotly({
    p3 <- parkrun_res %>%
      mutate(Event = as.factor(`Event `)) %>%
      arrange(Date) %>%
      group_by(Date) %>%
      summarise(event_count = n()) %>%
      ungroup() %>%
      mutate(Parkruns_completed = cumsum(event_count)) %>%
      ggplot(aes(x = as.Date(Date), y = Parkruns_completed)) +
      geom_hline(yintercept = 100)+
      annotate("text", x = as.Date("2017-06-15"), y = 104, label = "100th parkrun - Aug 2018", family = "Futura", size = 3)+
      geom_hline(yintercept = 200)+
      annotate("text", x = as.Date("2022-11-1"), y = 204, label = "200th parkrun - Dec 2023", family = "Futura", size = 3)+
      geom_path(colour = "red",linewidth = 2, lineend = "round")+
      #geom_area(fill = "#FFBE0B") +
      #geom_point(colour = "#3A86FF", alpha = 0.5, aes(size = 0.5))+
      theme_minimal() +
      labs(
        x = "",
        y = "Count"
      )+
      scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
      theme(axis.title = element_markdown(
        margin = margin(0, 0, 10, 0), 
        size   = rel(.9),
        color  = "black", 
        family        = "Futura",
        face   = "bold"
      ),
      legend.title = element_markdown(
        size   = rel(.9), 
        hjust  = 0.5,
        family        = "Futura",
        face   = "bold",
        color  = "black"
      ),
      legend.text = element_text(
        size   = rel(.7),
        family        = "Futura",
        face   = "bold",
        color  = "black"
      ),
      legend.key.height  = unit(.8, "cm"),
      #legend.margin    = margin(t = 5, b = -5, unit = "pt"), #size of the box
      plot.title      = element_text(
        size          = rel(2.1),
        family        = "Futura",
        face          = "bold",
        color         = "black",
        lineheight    = 1.1,
        margin        = margin(t = 5, b = 1)
      ),
      plot.subtitle   = element_textbox_simple(
        size          = rel(1.1), 
        family        = "Futura",
        color         = "black",
        lineheight    = 1.4, 
        margin        = margin(t = 2, b = 8)
      ),
      axis.text  = element_text(size = rel(.8),family        = "Futura"),
      axis.title.x  = element_text(size = rel(.9),family        = "Futura",face="bold", hjust = 0.5, vjust = -5),
      axis.title.y  = element_text(size = rel(1),family        = "Futura",face="bold", hjust = 0.5, vjust = 2),
      panel.grid.major.y = element_line(colour = "#ced4da", linewidth = 0.5),
      panel.grid.minor = element_blank())
    
    p3 |> ggplotly(tooltip = c("Parkruns_completed", "Date"), height = 600)
    
    
  })

}



shinyApp(ui, server)
