#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This file is used to develop new sim results
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: general scratch file
#-------- [[insert brief readme here]]
#
# *please use 80 character margins
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
library(tidyverse)
library(ggseas)
library(data.table)
library(lubridate)
library(DT)
library(plotly)
library(crosstalk)
library(janitor)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
source(here::here("script_dev/helpers_general.r"))
source(here::here("script_dev/helpers_plotly.r"))
source(here::here("script_dev/helpers_DT.r"))
source(here::here("R/bus_capcity_helpers.r"))

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts

#needs to be written out
sim #get this from the observe statement - simulation has to be ran 

data = sim[[3]]

#load the data here=============================================================
data = readRDS(here::here("data_dev/data_sim_saved.rds"))

#VPS bucket data exploration====================================================
#notes VPS and ICRS are not in a tidy format
#NOTE:: this first section I pivot all columns that have ICRS_#### prefix
#---> this is actually incorrect, look at section below

bind_rows(get_summry_statistics(sim[[3]]),
          get_summry_statistics(sim[[3]], grouped = T, group = "bus_line")) %>%
  mutate(bus_line = case_when(is.na(bus_line) ~ "All Buses",
                              T ~ bus_line)) %>%
  select(bus_line, everything()) %>%
  dt_common(dom = "Bftir",
            y = 600, pl = 8000)

#simulation visualizations======================================================
#two ways to do it - summaries each variabvle and bus over all simulations or 
#---- disuplay wach simulation individually 
#i think the latter is overkill but it is inluded as option #2

#OPTION #1_folding simulaiton results in with each other
#make data
bolo = data %>%
  select(bus_line, bus_line_id, starts_with("bus")) %>%
  select(!bus_route_door_cond ) %>% 
  unique() %>%
  pivot_longer(cols = !c(bus_line:bus_id)) %>%  
  select(!c(bus_line_id, bus_id))  #%>%  
  arrange(name, bus_line )

bolo_density = get_grouped_density(data = bolo, grp = c(bus_line, name))

#make plots
#these two plots do not use the same data
#they cannot be linked via sharedData
#probably best to put them side-by-side or in a tab box
#might be able to cache plots and then can use an input
bolo %>%  
  plot_ly(x = ~bus_line, y = ~value, color = ~bus_line,
          type = "box", boxmean = T,
          transforms = list(
            list(type = 'filter', target = ~name, operation = '=',
                 value = unique(bolo$name)[1]
            )
          )) %>%
  layout(xaxis = list(title = "Variable"), yaxis = list(title = "Density"),
         updatemenus =
           list(
             make_menu_item(name_list = unique(bolo$name), filter_pos = 0,
                            direction = "down", x = -0.5, y = 1.1)[[1]]
             
           ),
         showlegend = T)

bolo_density %>%
  plot_ly(x = ~units, y = ~density, fill = 'tozeroy',#text = ~text,
          type = 'scatter', mode = 'lines', 
          color = ~bus_line,
          transforms = list(
            list(type = 'filter', target = ~name, operation = '=',
                 value = unique(bolo$name)[1]
            )
          )) %>%
  layout(xaxis = list(title = "Variable"),
         yaxis = list(title = "Density"),
         updatemenus =
           list(
             make_menu_item(name_list = unique(bolo_density$name), filter_pos = 0,
                            direction = "down", x = -0.5, y = 1.1)[[1]]
             
           ),
         showlegend = T)  %>%
  highlight(on = "plotly_hover", off = "plotly_doubleclick")



#$$$$$$$$$$$$$$$$$$$
bolo = data %>%
  select(bus_line, bus_line_id, simulation_num, starts_with("bus")) %>%
  select(!bus_route_door_cond ) %>% 
  unique() %>%
  
  pivot_longer(cols = !c(bus_line:bus_id)) %>%  
  select(!c(bus_line_id, bus_id))  


bolo_density = get_grouped_density(data = bolo, grp = c(bus_line, name, simulation_num))

bolo_density$name %>%  unique()

yes = bolo_density %>%
  filter(name == "bus_service_time"
         ,simulation_num == 1
  ) %>% 
  arrange(bus_line, units) %>% 
  ggplot() + 
  geom_point(aes(units, density, group = simulation_num, color = bus_line)) + 
  coord_cartesian(xlim = c(210, 240))

yes %>%  ggplotly()

bolo_density %>%
  filter( name == "bus_service_time"
          ,simulation_num == 1|2
  ) %>%
  plot_ly(type = 'scatter', color = ~bus_line, opacity = .8) %>%  
  group_by(simulation_num) %>%  
  add_lines(x = ~units, y = ~density)

bolo %>%  
  plot_ly(x = ~bus_line, y = ~value, color = ~bus_line,
          type = "box", boxmean = T,
          transforms = list(
            list(type = 'filter', target = ~name, operation = '=',
                 value = unique(bolo$name)[1]
            )
          )) %>%
  layout(xaxis = list(title = "Variable"), yaxis = list(title = "Density"),
         updatemenus =
           list(
             make_menu_item(name_list = unique(tmp$name), filter_pos = 0,
                            direction = "down", x = -0.5, y = 1.1)[[1]]
             
           ),
         showlegend = T)

bolo_density %>%
  plot_ly(x = ~units, y = ~density, fill = 'tozeroy',#text = ~text,
          type = 'scatter', mode = 'lines', 
          color = ~bus_line,
          transforms = list(
            list(type = 'filter', target = ~name, operation = '=',
                 value = unique(bolo$name)[1]
            )
          )) %>%
  layout(xaxis = list(title = "Variable"),
         yaxis = list(title = "Density"),
         updatemenus =
           list(
             make_menu_item(name_list = unique(tmp$name), filter_pos = 0,
                            direction = "down", x = -0.5, y = 1.1)[[1]]
             
           ),
         showlegend = T)  %>%
  highlight(on = "plotly_hover", off = "plotly_doubleclick")


#script end=====================================================================

