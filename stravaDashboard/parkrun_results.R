library(tidyverse)
library(lubridate)
library(readxl)
library(ggsci)
library(ggthemes)
library(ggtext)

parkrun_results <- read_excel("stravaDashboard/parkrun_results.xlsx", 
                              col_types = c("text", "date", "numeric", 
                                            "text", "text"))
View(parkrun_results)

saveRDS(parkrun_results, here::here("parkrun_acts_2024-05-28.rds"))

# change time as character to time as time using ms function
parkrun_results <- parkrun_results %>%
  rowwise() %>%
  mutate(timer_period = ms(Time),
         decimal_time = as.duration(timer_period) / dminutes(1))

as.duration(parkrun_results$timer_period) / dminutes(1) # this changes period to decimal of mins.

col_palette  <- c("Preston Park" = "#3F88C5", "Worthing" = "#D00000", "Edgbaston Reservoir" = "#FFBA08", "Cannon Hill" = "#136F63", "Hove Promenade" = "#032b43")

ggplot(parkrun_results, aes(x = as.Date(Date), y = round(decimal_time,1)))+
  geom_point(aes(colour = parkrun_results$`Event `), alpha = 0.5)+
  geom_vline(aes(xintercept = as.Date("2020-03-18")))+
  geom_vline(aes(xintercept = as.Date("2021-07-24")))+
  annotate("text", x = as.Date("2020-12-1"), y = 25, label = "All events\nsuspended\ndue to\nCovid-19",family = "Futura")+
  scale_color_manual(values = col_palette)+
  scale_fill_manual(values = col_palette)+
  theme_fivethirtyeight()+
  labs(
    x        = "Date",
    y        = "Time (mins)",
    title    = "Parkrun results over time",
    colour = "Location")+
  scale_x_date(date_breaks = "1 year", date_labels = "%b-%Y")+
  scale_y_continuous(breaks = seq(20, max(round(parkrun_results$decimal_time,1)), by = 1))+
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
        axis.title.x  = element_text(size = rel(1.3),family        = "Futura",face="bold", hjust = 0.5, vjust = -5),
        axis.title.y  = element_text(size = rel(1),family        = "Futura",face="bold", hjust = 0.5, vjust = 2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "#ced4da", linewidth = 0.5))


ggplot(parkrun_res, aes(x = `Event `, y = round(decimal_time,1)))+
  geom_jitter(alpha = 0.3)+
  geom_boxplot()+
  labs(
    x        = "Event",
    subtitle = "dddd",
    y        = "Time (mins)",
    colour = "Location")+
  scale_y_continuous(breaks = seq(20, max(round(parkrun_res$decimal_time,1)), by = 1))


ggplot(parkrun_res, aes(x = floor(decimal_time)))+
  geom_histogram(binwidth = 1)+
  labs(
    x        = "Time (decimal mins)",
    subtitle = "dddd",
    y        = "Count",
    colour = "Location")+
  scale_x_continuous(breaks = seq(19, 30, by = 1))+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank())+
  facet_wrap(~`Event `, scales = 'free_x')

library(ggstream)

parkrun_res %>%
  mutate(Event = as.factor(`Event `)) %>%
  arrange(Date) %>%
  group_by(Date) %>%
  summarise(event_count = n()) %>%
  ungroup() %>%
  mutate(cumulative_event_count = cumsum(event_count)) %>%
  ggplot(aes(x = as.Date(Date), y = cumulative_event_count)) +
  geom_area() +
  theme_minimal()+
  labs(
    x = "Date",
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
  panel.grid.major = element_blank(),
  panel.grid.minor = element_line(colour = "#ced4da", linewidth = 0.5))

# to do - inner merge with strava data to find all activities that were parkruns. extract, associated data for that and build dashboard using above plots. parkrun locations, time of year, etc.


my_acts2 <- my_acts2 |> 
  mutate(start_dttm = as_datetime(x = start_date_local),
         start_time = format(start_dttm, "%H:%M:%S"))

my_acts2$Date <- date(my_acts2$Date) 

#filter Strava activities so there's only one match per parkrun event. filter the 
starttime <- hms("08:55:00")
endtime <- hms("09:07:00")

df <- my_acts2 |> dplyr::filter(sport_type == "Run" & 
                                  hms(format(start_dttm, "%H:%M:%S")) > starttime &
                                  hms(format(start_dttm, "%H:%M:%S")) < endtime) |> 
  inner_join(parkrun_results,my_acts2, by = join_by(Date))



df <- my_acts2 |> dplyr::filter(sport_type == "Run" & hour(start_dttm) >= 8 & hour(start_dttm) <= 9 &
                                  hms(format(start_dttm, "%H:%M:%S")) > start_time &
                                  hms(format(start_dttm, "%H:%M:%S")) < end_time) |> 
  inner_join(parkrun_results,my_acts2, by = join_by(Date))
