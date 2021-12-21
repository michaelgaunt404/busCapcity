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
# source(here::here("script_dev/helpers_general.r"))
source(here::here("r/helpers_plotly.r"))
source(here::here("r/utils_helpers_general.r"))
# source(here::here("script_dev/helpers_DT.r"))
source(here::here("R/bus_capcity_helpers.r"))
source(here::here("R/golem_utils_server.r"))


input = list(cols_to_pivot = cols_to_pivot, 
             hist_transfrom = "identity", 
             hist_binwidth = 10, 
             arrival_plot_color = "Bus Line*bus_line",
             arrival_plot_color = "Bus Experiences \nEntry Delay*bus_delay_entry>1"
)   

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts

#needs to be written out
sim #get this from the observe statement - simulation has to be ran 

data = sim[[3]]

data %>%  
  clipr::write_clip()

#load the data here=============================================================
data = readRDS(here::here("data_dev/data_sim_saved.rds"))
varibles_list = read.csv("./data_dev/variables_list.csv")

index_resultPlot = varibles_list %>%  
  filter(Exclude != "X")   

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
#two ways to do it - summaries each variable and bus over all simulations or 
#---- display was simulation individually 
#i think the latter is overkill but it is included as option #2

#OPTION #1_folding simulation results in with each other
#make data
bolo = data %>%
  select(bus_line, bus_line_id, starts_with("bus")) %>%
  select(!bus_route_door_cond ) %>%
  unique() %>%
  pivot_longer(cols = !c(bus_line:bus_id)) %>%
  select(!c(bus_line_id, bus_id))

bolo_density = get_grouped_density(data = bolo, grp = c(bus_line, name))

bolo = data %>%
  select(bus_line, bus_line_id, bus_id, index_resultPlot$names_raw) %>% 
  unique() %>%
  pivot_longer(cols = !c(bus_line:bus_id)) %>%  
  merge(., index_resultPlot, by.x = "name", by.y = "names_raw") %>%  
  select(bus_line, bus_line_id, names_p1, value) %>%  
  arrange(bus_line, names_p1)

bolo_density = get_grouped_density(data = bolo, grp = c(bus_line, names_p1))




#make plots
#these two plots do not use the same data
#they cannot be linked via sharedData
#probably best to put them side-by-side or in a tab box
#might be able to cache plots and then can use an input
bolo %>%  
  plot_ly(x = ~bus_line, y = ~value, color = ~bus_line,
          type = "box", boxmean = T,
          transforms = list(
            list(type = 'filter', target = ~names_p1, operation = '=',
                 value = unique(bolo$names_p1)[1]
            )
          )) %>%
  layout(xaxis = list(title = ""), 
         yaxis = list(title = ""),
         updatemenus =
           list(
             make_menu_item(name_list = unique(bolo$names_p1), filter_pos = 0, type = "buttons",
                            direction = "down", x = -0.5, y = 1.1)[[1]]
             
           ),
         showlegend = T)

bolo %>%  
  plot_ly(x = ~bus_line, y = ~value, color = ~bus_line,
          type = "box", boxmean = T,
          transforms = list(
            list(type = 'filter', target = ~names, operation = '=',
                 value = unique(bolo$names)[1]
            )
          )) %>%
  layout(xaxis = list(title = "yyy"), 
         yaxis = list(title = "uuuu"),
         updatemenus =
           list(
             make_menu_item(name_list = unique(bolo$names), filter_pos = 0, type = "buttons",
                            direction = "down", x = -0.5, y = 1.1)[[1]]
             
           ),
         showlegend = T)

bolo %>%  
  plot_ly(x = ~value, color = ~bus_line,
          type = "histogram", 
          transforms = list(
            list(type = 'filter', target = ~names_p1, operation = '=',
                 value = unique(bolo$names_p1)[1]
            )
          )) %>%
  layout(xaxis = list(title = "Variable"), 
         yaxis = list(title = ~names_p1),
         updatemenus =
           list(
             make_menu_item(name_list = unique(bolo$names_p1), filter_pos = 0, type = "buttons",
                            direction = "down", x = -0.5, y = 1.1)[[1]]
             
           ),
         showlegend = T)


bolo_density %>%
  plot_ly(x = ~units, y = ~density, fill = 'tozeroy',#text = ~text,
          type = 'scatter', mode = 'lines', 
          color = ~bus_line,
          transforms = list(
            list(type = 'filter', target = ~names_p1, operation = '=',
                 value = unique(bolo$names_p1)[1]
            )
          )) %>%
  layout(xaxis = list(title = "Variable"),
         yaxis = list(title = "Density"),
         updatemenus =
           list(
             make_menu_item(name_list = unique(bolo_density$names_p1), filter_pos = 0, type = "buttons",
                            direction = "down", x = -0.5, y = 1.1)[[1]]
             
           ),
         showlegend = T)  %>%
  highlight(on = "plotly_hover", off = "plotly_doubleclick")



#OPTION #2 
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




temp = sim[[3]]  %>%
  mutate(bus_total_operation = bus_delay_entry  + bus_service_time + bus_delay_exit) %>% 
  select(bus_line, bus_line_id, bus_id, index_resultPlot$names_raw) %>% 
  unique() %>%
  pivot_longer(cols = !c(bus_line:bus_id)) %>%  
  merge(., index_resultPlot, by.x = "name", by.y = "names_raw") %>%  
  select(bus_line, bus_line_id, names_p1, value) %>%  
  arrange(bus_line, names_p1) 
  

temp %>%  
  plotly::plot_ly(x = ~bus_line, y = ~value, color = ~bus_line,
                  type = "box", boxmean = T,
                  transforms = list(
                    list(type = 'filter', target = ~names_p1, operation = '=',
                         value = unique(temp$names_p1)[1]
                    )
                  )) %>%
  plotly::layout(xaxis = list(title = ""), 
                 yaxis = list(title = "", rangemode = "tozero"),
                 updatemenus =
                   list(
                     make_menu_item(name_list = unique(temp$names_p1), filter_pos = 0
                                    ,xanchor = "left" ,yanchor = "top" ,x = 1 ,y = 1
                                    ,direction = "down" ,type = "dropdown"
                     )[[1]]
                   )
                 ,legend = list(orientation = "h"
                                ,x = 0 ,y = 1.1)
                 ,showlegend = T)


#total wait visualizations=======================================================

data %>%  
  select(-starts_with("pass_")) %>%
  unique() %>% 
  # select(where(is.numeric)) %>% 
  mutate(check = bus_service_time + bus_delay_exit) %>%
  mutate(check1 = bus_delay_entry  + bus_service_time + bus_delay_exit)
  mutate(service_time_ratio = bus_service_time_alight/bus_service_time_board,
         delay_total_ratio = bus_delay_entry/)
  mutate(across())
  # filter(simulation_num == 1) %>%  
  # clipr::write_clip()
  # unique() %>% 
  ggplot() + 
  geom_point(aes(bus_time_inBerth , bus_delay_total)) + 
  xlim(0, 120)
    
  data %>%  
    select(bus_pass_alight, bus_service_time_alight, bus_delay_entry, bus_service_time_board) %>% 
    # select(-starts_with("pass_")) %>% 
    unique() %>% 
    # select(where(is.numeric)) %>% 
    cor() %>% 
    corrplot::corrplot()
  # filter(simulation_num == 1) %>%  
  # clipr::write_clip()
  # unique() %>% 
  ggplot() + 
    geom_point(aes()) 

  
  data %>%  
    select(-starts_with("pass_")) %>%
    unique() %>%  
    mutate(index = dplyr::row_number(), 
           sum =  bus_delay_entry  + bus_service_time + bus_delay_exit) %>% 
    pivot_longer(cols = c( bus_delay_entry, bus_service_time, bus_delay_exit)) %>%  
    select(index, name, value) %>%  
    unique() %>% 
    ggplot() +
    # + geom_tile(aes(index, name, fill = value ))
  geom_line(aes(index, value, color = name))
  geom_col(aes(index, value, fill = name))
    
    
    
    
    



#script end=====================================================================

