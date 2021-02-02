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

local_gtfs_path <- system.file("extdata", 
                               "google_transit_nyc_subway.zip", 
                               package = "tidytransit")


nyc <- read_gtfs(local_gtfs_path)
nyc2 <- read_gtfs("http://developer.onebusaway.org/tmp/sound/gtfs/modified/gtfs_puget_sound_consolidated.zip")

sea = nyc2

nyc$calendar_dates$date

head(feedlist)
view(feedlist)
feedlist %>%  
  filter(str_detect(loc_t, "Seat"))



