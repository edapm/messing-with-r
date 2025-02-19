# Testing out tidytransit
library(tidytransit)
library(tidyverse)
library(sf)
rm(list=ls())

# load gtfs feed and get a summary
nyc <- read_gtfs("http://web.mta.info/developers/data/nyct/subway/google_transit.zip")
summary(nyc)

# print stops
head(nyc$stops)

# print service patterns
head(nyc$.$dates_services)

# set service pattern and print info
gtfs <- set_servicepattern(nyc)
head(gtfs$.$servicepatterns)
head(gtfs$.$dates_servicepatterns)

# set some dates
holidays = tribble(~date, ~holiday,
                   ymd("2024-07-04"), "Independence Day",
                   ymd("2024-09-03"), "Labor Day")

calendar = tibble(date = unique(gtfs$.$dates_services$date)) %>% 
  mutate(
    weekday = (function(date) {
      c("Sunday", "Monday", "Tuesday", 
        "Wednesday", "Thursday", "Friday", 
        "Saturday")[as.POSIXlt(date)$wday + 1]
    })(date)
  )

calendar <- calendar %>% left_join(holidays, by = "date")
head(calendar)

# number of service ids used
n_services <- length(unique(gtfs$trips$service_id)) # 52

# unique date patterns 
n_servicepatterns <- length(unique(gtfs$.$servicepatterns$servicepattern_id)) # 3

# plot service patterns over holidays
date_servicepattern_table <- gtfs$.$dates_servicepatterns %>% left_join(calendar, by = "date")

ggplot(date_servicepattern_table) + theme_bw() + 
  geom_point(aes(x = date, y = servicepattern_id, color = weekday), size = 1) + 
  scale_x_date(breaks = scales::date_breaks("1 month")) + theme(legend.position = "bottom")

# plot service routes as graphs
gtfs2 <- set_servicepattern(nyc)
gtfs2 <- gtfs_as_sf(gtfs2)
gtfs2$shapes$length <- st_length(gtfs2$shapes)

shape_lengths <- gtfs2$shapes %>% 
  as.data.frame() %>% 
  select(shape_id, length, -geometry)

# summarise
service_pattern_summary <- gtfs$trips %>%
  left_join(gtfs$.$servicepatterns, by="service_id") %>% 
  left_join(shape_lengths, by="shape_id") %>%
  left_join(gtfs$stop_times, by="trip_id") %>% 
  group_by(servicepattern_id) %>% 
  summarise(
    trips = n(), 
    routes = n_distinct(route_id),
    total_distance_per_day_km = sum(as.numeric(length), na.rm=TRUE)/1e3,
    route_avg_distance_km = (sum(as.numeric(length), na.rm=TRUE)/1e3)/(trips*routes),
    stops=(n_distinct(stop_id)/2))

# add number of days in op
service_pattern_summary <- gtfs2$.$dates_servicepatterns %>% 
  group_by(servicepattern_id) %>% 
  summarise(days_in_service = n()) %>% 
  left_join(service_pattern_summary, by="servicepattern_id")

service_ids <- gtfs2$.$servicepatterns %>% 
  filter(servicepattern_id == 's_0fb7ac8') %>% 
  pull(service_id)

gtfs2$trips %>%
  filter(service_id %in% service_ids) %>%
  group_by(service_id, route_id) %>%
  summarise(count = n()) %>% 
  head()

am_stop_freq <- get_stop_frequency(gtfs2, start_time = 6*3600, end_time = 10*3600, 
                                   service_ids = service_ids, by_route = TRUE)

one_line_stops <- am_stop_freq %>% 
  filter(route_id == "1" & direction_id == 0) %>%
  left_join(gtfs2$stops, by ="stop_id") %>% 
  mutate(mean_headway_minutes = mean_headway/60)

one_line_stops_sf <- gtfs2$stops %>%
  right_join(one_line_stops, by="stop_id") 

one_line_stops_sf %>% 
  ggplot() + 
  geom_sf(aes(color = mean_headway_minutes)) +
  theme_bw()

summary(one_line_stops$mean_headway)

# mapping
am_route_freq <- get_route_frequency(gtfs2, service_ids = service_ids, 
                                     start_time = 6*3600, end_time = 10*3600) 
head(am_route_freq)
routes_sf <- get_route_geometry(gtfs2, service_ids = service_ids)
routes_sf <- routes_sf %>% 
  inner_join(am_route_freq, by = 'route_id')
# convert to an appropriate coordinate reference system
routes_sf_crs <- sf::st_transform(routes_sf, 26919) 
routes_sf_crs %>% 
  filter(median_headways < 10*60) %>%
  ggplot() + 
  geom_sf(aes(colour=as.factor(median_headways))) + 
  labs(color = "Headways") +
  geom_sf_text(aes(label=route_id)) +
  theme_bw()
