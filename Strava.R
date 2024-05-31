library(tidyverse)
library(leaflet)
library(rStrava)
library(sf)
library(httr)

app_name <- "" # chosen by user
app_client_id  <- "" # an integer, assigned by Strava
app_secret <- "" # an alphanumeric secret, assigned by Strava

stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret, cache = TRUE, app_scope="activity:read_all"))

stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])

strava_token <- httr::config(token = strava_oauth(app_name,
                                                  app_client_id,
                                                  app_secret,
                                                  app_scope = "activity:read_all"))
my_acts <- get_activity_list(strava_token) 
id = 10695060340
strava_data <- get_activity_streams(my_acts,
                                    strava_token,
                                    id = id)
strava_data
write.csv(strava_data, "strava_data.csv", row.names = F)
library(tidyverse)
strava_data %>% 
  as_tibble() %>% 
  select(-id)

library(sf)
library(ggmap)
library(osmdataa)
library(rcartocolor)
library(gganimate)


bb <- c(-0.4465567, -0.3302629, 50.8017364, 50.8632589)


bg_map <- get_map(location = bb,
                  source = "stadia",
                  maptype = "stamen_terrain", 
                  color = "bw",
                  scale = 4)

register_google(key = "AIzaSyDPYVyRPbqAUYqxeqlODV7TaKiONCQKjXA")


ggmap(bg_map)

