leaflet_map <- function(filtered_acts, filtered_run_data) {
  # Create a new column for the marker color based on the event
  filtered_run_data$marker_color <- case_when(
    stringr::str_detect(filtered_run_data$event, "Edgbaston Reservoir|Cannon Hill") ~ "#D00000",
    filtered_run_data$event == "Worthing" ~ "#136f63",
    stringr::str_detect(filtered_run_data$event, "Preston Park|Hove Promenade") ~ "#3f88c5",
    TRUE ~ "black"
  )
  
  leaflet() |>
    addProviderTiles(providers$Stadia.OSMBright, group = "",
                     options = providerTileOptions(maxZoom = 18)) |>
    setView(lng = -1.178644173785564, lat = 51.84033518449337, zoom = 7) |>
    addCircleMarkers(data = filtered_run_data,
                     group = "Display Run Icons",
                     lng = ~jitter(lng, factor = 0.01),
                     lat = ~jitter(lat, factor = 0.01),
                     popup = paste0("<strong>Run Title:</strong> ", filtered_run_data$name, "<br>",
                                    "<strong>Distance (km):</strong> ", filtered_run_data$total_km, "<br>",
                                    "<strong>Date:</strong> ", filtered_run_data$Date),
                     radius = 10, color = "white", fillColor = ~marker_color, fillOpacity = 0.5) |>
    addResetMapButton() -> map
  
  unique_acts_ids <- unique(filtered_acts$id)
  
  map <- lapply(unique_acts_ids, function(i) {
    activity <- dplyr::filter(filtered_acts, id == i)
    if (activity$map.summary_polyline != "") {
      coords <- googleway::decode_pl(activity$map.summary_polyline)
    } else {
      coords <- NULL
    }
    
    if (!is.null(coords) && !is.null(coords$lon) && !is.null(coords$lat)) {
      color <- case_when(
        stringr::str_detect(activity$event, "Edgbaston Reservoir|Cannon Hill") ~ "#D00000",
        activity$event == "Worthing" ~ "#136f63",
        stringr::str_detect(activity$event, "Preston Park|Hove Promenade") ~ "#3f88c5",
        TRUE ~ "black"
      )
      
      map <- addPolylines(map, lng = coords$lon, lat = coords$lat,
                          color = color, fillOpacity = 0.9, weight = 2)
    }
    
    map
  })
  
  map <- map[[length(map)]]
  
  map <- addLegend(map, position = "topright",
                   title = "Event",
                   colors = c("#D00000", "#136f63", "#3f88c5"),
                   labels = c("Edgbaston Reservoir & Cannon Hill", "Worthing", "Preston Park & Hove Promenade"))
  
  map
}
