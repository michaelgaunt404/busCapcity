#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Look at tidytransit package
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: NA
#-------- NA
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidytransit)
library(tidyverse)
library(sf)
library(leaflet)
library(htmltools)

local_gtfs_path <- system.file("extdata", 
                               "google_transit_nyc_subway.zip", 
                               package = "tidytransit")
gtfs_nyc <- read_gtfs(local_gtfs_path)
gtfs_nyc <- set_servicepattern(gtfs_nyc)
shp1 <- shapes_as_sf(gtfs_nyc$shapes)
shp1 <- st_transform(shp1, crs=2263)
shp1$length <- st_length(shp1)
shp2 <- shp1 %>% 
  as.data.frame() %>% 
  select(shape_id,length,-geometry) 

service_pattern_summary <- gtfs_nyc$trips %>%
  left_join(gtfs_nyc$.$servicepatterns, by="service_id") %>% 
  left_join(shp2, by="shape_id") %>%
  left_join(gtfs_nyc$stop_times, by="trip_id") %>% 
  group_by(servicepattern_id) %>% 
  summarise(trips = n(), 
            routes = n_distinct(route_id),
            total_distance_per_day_km = sum(as.numeric(length), 
                                            na.rm=TRUE)/1e3,
            route_avg_distance_km = (sum(as.numeric(length),
                                         na.rm=TRUE)/1e3)/(trips*routes),
            stops=(n_distinct(stop_id)/2))

service_pattern_summary <- gtfs_nyc$.$dates_servicepatterns %>% 
  group_by(servicepattern_id) %>% 
  summarise(days_in_service = n()) %>% 
  ungroup() %>% 
  left_join(service_pattern_summary, by="servicepattern_id")

service_ids <- gtfs_nyc$.$servicepatterns %>% 
  filter(servicepattern_id == 's_e25d6ca') %>% 
  pull(service_id)

am_freq <- get_stop_frequency(gtfs_nyc, start_hour = 6, end_hour = 10, service_ids = service_ids, by_route = T)

one_line_stops <- am_freq %>% 
  filter(route_id==1 & direction_id==0) %>%
  left_join(gtfs_nyc$stops, by ="stop_id")

one_line_stops %>% 
  arrange(desc(mean_headway)) %>% 
  select(stop_name, n_departures , mean_headway) %>% 
  head() 
tidytransit::get
am_route_freq <- get_route_frequency(gtfs_nyc, service_ids = service_ids, start_hour = 6, end_hour = 10) 
head(am_route_freq) %>%
  knitr::kable()

s_e25d6ca

gtfs_nyc$.$servicepatterns
gtfs$.$dates_servicepatterns




gtfs <- set_servicepattern(gtfs)
sea = nyc2

nyc$calendar_dates$date

head(feedlist)
view(feedlist)



validation_result <- attr(nyc, "validation_result")
head(validation_result)


feedlist_sf <- st_as_sf(feedlist,
                        coords=c("loc_lng","loc_lat"),
                        crs=4326)

mapview::mapview()

feedlist_sf %>%  
  st_jitter(factor = .0001) %>% 
  leaflet() %>%
  addTiles() %>% 
  addCircles(popup = ~str_glue("{loc_t}\n{url_i}"))

WS_ferry = feedlist %>%  
  filter(str_detect(loc_t, "Seat") & str_detect(t, "GTFS")) %>%  
  filter(t == "King County Metro GTFS") %>%  
  pull(url_d) %>%  
  read_gtfs()

gtfs = WS_ferry

gtfs <- set_servicepattern(gtfs)

shp1 <- shapes_as_sf(gtfs$shapes)
# shp1 <- st_transform(shp1, crs=2263)
shp1$length <- st_length(shp1)

shp1 %>%  
  plot()
  leaflet() %>%  
  addTiles() %>%  
  addPolylines()


shp2 <- shp1 %>% 
  as.data.frame() %>% 
  select(shape_id,length,-geometry) 

service_pattern_summary <- gtfs$trips %>%
  left_join(gtfs$.$servicepatterns, by="service_id") %>% 
  left_join(shp2, by="shape_id") %>%
  left_join(gtfs$stop_times, by="trip_id") %>% 
  group_by(servicepattern_id) %>% 
  summarise(trips = n(), 
            routes = n_distinct(route_id),
            total_distance_per_day_km = sum(as.numeric(length), 
                                            na.rm=TRUE)/1e3,
            route_avg_distance_km = (sum(as.numeric(length),
                                         na.rm=TRUE)/1e3)/(trips*routes),
            stops=(n_distinct(stop_id)/2))

service_pattern_summary <- gtfs$.$date_servicepattern_table %>% 
  group_by(servicepattern_id) %>% 
  summarise(days_in_service = n()) %>% 
  left_join(service_pattern_summary, by="servicepattern_id")

service_ids <- gtfs$.$servicepatterns %>% 
  filter(servicepattern_id == 's_c1af903') %>% 
  pull(service_id)


am_freq <- get_stop_frequency(gtfs, start_hour = 6, end_hour = 10, service_ids = service_ids)


























