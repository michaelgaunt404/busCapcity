# install.packages("magrittr")
# library(magrittr)
# list("thematic" ) %>%  
#   lapply(install.packages)
# 
# 

library(tidyverse)
library(scales)
library(plotly)








# saveRDS(data_sim_saved,
      # here::here("data_dev/data_sim_saved.rds"))
sim


bind_rows(get_summry_statistics(sim[[3]]),
          get_summry_statistics(sim[[3]], grouped = T, group = "bus_line")) %>%
  mutate(bus_line = case_when(is.na(bus_line) ~ "All Buses",
                              T ~ bus_line)) %>%
  select(bus_line, everything()) %>%
  dt_common(dom = "Bftir",
            y = 600, pl = 8000)







#need these inputs==============================================================
input = list(cols_to_pivot = cols_to_pivot, 
             hist_transfrom = "identity", 
             hist_binwidth = 10, 
             arrival_plot_color = "Bus Line*bus_line",
             arrival_plot_color = "Bus Experiences \nEntry Delay*bus_delay_entry>1"
)            
             
#load the data here=============================================================
data = readRDS(here::here("data_dev/data_sim_saved.rds"))

#sets-up shiny input simulation=================================================
cols_to_pivot = c(#"bus_service_time_alight",
  "bus_surplus_seats", "bus_delay_entry",
  "bus_service_time",
  # "bus_time_inBerth",
  "bus_pass_picked_up")

#sets-up shiny input simulation=================================================
data %>%
  select(bus_line, bus_line_id, starts_with("bus")) %>%
  unique() %>%
  select(!bus_route_door_cond ) %>% 
  pivot_longer(cols = !c(bus_line:bus_id))

#sets-up shiny input simulation=================================================
data %>%
  select(bus_line, bus_line_id, starts_with("bus")) %>%
  unique() %>%
  select(!bus_route_door_cond ) %>% 
  pivot_longer(cols = !c(bus_line:bus_id)) %>% 

  ggplot() +
  geom_freqpoly(aes(value, color = bus_line), binwidth = input$hist_binwidth) +
  # geom_histogram(aes(Value, fill = bus_line), binwidth = input$hist_binwidth, position = "dodge2") +
  # geom_density(aes(Value, fill = bus_line), alpha = .5) +
  # facet_grid(rows = vars(Metrics), scales = "free") +
  facet_wrap(facets = vars(name), ncol = 2, scales = "free") +
  scale_y_continuous(trans = input$hist_transfrom)





bolo %>%  plotly::ggplotly()

yolo = 
  data %>%  
  filter(simulation_num == 1) %>%
  select(bus_line, bus_line_id, starts_with("bus")) %>%
  unique() %>%  
  mutate(order = dplyr::row_number(), 
         bus_line_id = fct_inorder(bus_line_id), 
         text = str_glue("Bus Line/ID: {bus_line_id}
                         Passengers Alighted/Boarded: {bus_pass_alight}/{bus_pass_picked_up}
                         Total Entry Delay {round(bus_delay_entry/60,1)} min
                         Total Exit Delay {round(bus_delay_exit/60,1)} min
                         Alight/Board/Total Serv. Time: {round(bus_service_time_alight/60,1)}/{round(bus_service_time_board/60,1)}/{round(bus_service_time/60,1)} min")) 
yolo_plot = yolo %>% 
  ggplot() + 
  geom_segment(aes_string(x = 'bus_arrvl_actl', xend = 'bus_exit_actual', 
                   y = 'bus_line_id', yend = 'bus_line_id', 
                   color = gsub(".*\\*", "\\1", input$arrival_plot_color), 
                   text = "text"), size = 2, alpha = .3) +
    labs(color = gsub("\\*.*", "\\1",input$arrival_plot_color)) + 
  scale_x_continuous(trans = 'hms', 
                     breaks =  scales::breaks_width("15 min"),
                     labels = scales::label_time(format = "%H:%M:%S"))

ggplotly(yolo_plot, tooltip = "text") 

tmp = data %>%  
  select(bus_line_id, bus_arrvl_actl, simulation_num, starts_with("bus_delay_")) %>% 
  unique() %>% 
  pivot_longer(cols = starts_with("bus_delay_")) %>% 
  filter(!str_detect(name, "_total"))

tmp %>%  
  filter(simulation_num == 6, 
         !str_detect(name, "_total")) %>% 
  plot_ly(x = ~bus_line_id, y = ~value, color = ~name, type = 'bar') %>% 
  layout(yaxis = list(title = 'Count'), barmode = 'stack')

tmp_new = tmp %>%  
  filter(!str_detect(name, "_total")) %>% 
  SharedData$new()

tmp %>%  
  filter(simulation_num == sample(seq(1, 20), 1)) %>% 
  mutate(bus_line_id = fct_reorder(bus_line_id, bus_arrvl_actl) ) %>% 
  plot_ly(x = ~bus_line_id, y = ~value, color = ~name, type = 'bar') %>% 
  layout(yaxis = list(title = 'Count'), barmode = 'stack')


bscols(
  widths = c(3,9),
  filter_slider("mag", "Magnitude", tmp_new, column=~simulation_num , 
                step=1, width=250, animate = T),
  tmp_new %>%  
    plot_ly(x = ~bus_line_id, y = ~value, color = ~name, type = 'bar') %>% 
    layout(yaxis = list(title = 'Count'), barmode = 'stack')
)

  ggplot() + 
  geom_col(aes(x = bus_line_id, y = value, fill = name)) +
  coord_flip()
  
  yolo %>%  
    select(!text) %>% 
    clipr::write_clip()









