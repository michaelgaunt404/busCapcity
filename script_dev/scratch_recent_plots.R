# install.packages("magrittr")
# library(magrittr)
# list("thematic" ) %>%  
#   lapply(install.packages)
# 
# 


library(scales)
library(plotly)








saveRDS(data_sim_saved,
      here::here("data_dev/data_sim_saved.rds"))
sim


bind_rows(get_summry_statistics(sim[[3]]),
          get_summry_statistics(sim[[3]], grouped = T, group = "bus_line")) %>%
  mutate(bus_line = case_when(is.na(bus_line) ~ "All Buses",
                              T ~ bus_line)) %>%
  select(bus_line, everything()) %>%
  dt_common(dom = "Bftir",
            y = 600, pl = 8000)




data_sim_saved = sim[[3]]
data = data_sim_saved
get_summry_statistics = function(data, grouped = F, group){
  
  if (!grouped){
    tmp_data = data %>%  
      select(bus_line, bus_line_id, starts_with("bus")) %>%
      unique() 
    
  } else {
    tmp_data = data %>%  
      select(bus_line, bus_line_id, starts_with("bus")) %>%
      unique() %>% 
      group_by(!!as.symbol(group)) 
  }
  
  tmp_data %>%
    summarise(across(all_of(list_fiveNum_cols), list_fiveNum_func, .names = names_glue),
              across(all_of(list_count_cols), list_count_func, .names = names_glue)) %>% 
    pivot_longer(cols = !starts_with("bus_line")) %>%  
    tidyr::separate(col = name, into = c("metric", "type"), sep = "\\.") %>%  
    mutate(across(value, round, 1)) %>% 
    pivot_wider(names_from = type, values_from = value)
  
}




             
             )
#load the data here=============================================================
# data = readRDS(
# here::here("data_dev/data_sim_saved.rds"))

#sets-up shiny input simulation=================================================
cols_to_pivot = c(#"bus_service_time_alight",
  "bus_surplus_seats", "bus_delay_entry",
  "bus_service_time",
  # "bus_time_inBerth",
  "bus_pass_picked_up")

input = list(cols_to_pivot = cols_to_pivot, 
             hist_transfrom = "identity", 
             hist_binwidth = 10, 
             # arrival_plot_color = "Bus Line*bus_line",
             arrival_plot_color = "Bus Experiences \nEntry Delay*bus_delay_entry>1"
)

#sets-up shiny input simulation=================================================
bolo = data %>%
  select(bus_line, bus_line_id, starts_with("bus")) %>%
  unique() %>%
  pivot_longer(cols = input$cols_to_pivot,
               names_to = "Metrics",
               values_to = "Value") %>%
  ggplot() +
  geom_freqpoly(aes(Value, color = bus_line), binwidth = input$hist_binwidth) +
  # geom_histogram(aes(Value, fill = bus_line), binwidth = input$hist_binwidth, position = "dodge2") +
  # geom_density(aes(Value, fill = bus_line), alpha = .5) +
  # facet_grid(rows = vars(Metrics), scales = "free") +
  facet_wrap(facets = vars(Metrics), ncol = 2, scales = "free") +
  scale_y_continuous(trans = input$hist_transfrom)

bolo %>%  plotly::ggplotly()

yolo = 
  data %>%  
  filter(simulation_num == 7) %>%
  select(bus_line, bus_line_id, starts_with("bus")) %>%
  unique() %>%  
  mutate(order = dplyr:=:row_number(), 
         bus_line_id = fct_inorder(bus_line_id), 
         text = str_glue("<strong>Bus Line/ID:</strong> {bus_line_id}
                         Passengers Alighted/Boarded: {bus_pass_alight}/{bus_pass_picked_up}
                         Total Entry Delay {round(bus_delay_entry/60,1)} min
                         Total Exit Delay {round(bus_delay_exit/60,1)} min
                         Alight/Board/Total Serv. Time: {round(bus_service_time_alight/60,1)}/{round(bus_service_time_board/60,1)}/{round(bus_service_time/60,1)} min",)) %>% 
  ggplot() + 
  geom_segment(aes_string(x = 'bus_arrvl_actl', xend = 'bus_exit_actual', 
                   y = 'bus_line_id', yend = 'bus_line_id', 
                   color = gsub(".*\\*", "\\1", input$arrival_plot_color), 
                   text = "text"), size = 2, alpha = .3) +
    labs(color = gsub("\\*.*", "\\1",input$arrival_plot_color)) + 
  scale_x_continuous(trans = 'hms', 
                     breaks =  breaks_width("15 min"),
                     labels = scales::label_time(format = "%H:%M:%S"))

ggplotly(yolo,tooltip = "text") 









