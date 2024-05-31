source("parkrun_results.R")
total_parkruns<- function(input) {
  
  total_parkruns <- parkrun_results |>
      nrow()
  }
  
  # render valuebox ----
  renderValueBox({
    valueBox("Total number of parkruns", value = total_filtered_rides(), color = "purple")
  })
  
