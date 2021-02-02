#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This script is a first attempt at creating PASSION model function
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: NA
#-------- NA
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#no alight - downstream impact of alight/board interaction through single door
#|-----> one may take longer even if they are parallel 


simul_time = 60 #in minutes
shiny_input_pass_headway_sd = 0 #in seconds - possion dist - better way of asking 
shiny_input_pass_board = 2.1 #in seconds - chekced in yingying's notes
shiny_input_pass_board_sd = 0 #in seconds
shiny_input_bus_exit_condition = 4 #this makes conditional - this needs to change gui inputs

#flow conditional 
shiny_input_veh_flow = 1800 #veh/hr
shiny_input_sat_flow = 1900 #veh/hr per gtime
#need value for critical gap - it appears to be a 

#route 1 inputs=================================================================

#inputs from gui - bus
shiny_input_bus_1_route = "506" #in minutes
shiny_input_bus_1_headway = 15 #in minutes
shiny_input_bus_1_headway_sd = 3 #in minutes
shiny_input_bus_1_capacity = 50 #in persons
shiny_input_bus_1_spare = .63 #in percent

#inputs from gui - pass
shiny_input_pass_1_num = 250 #total serviced for simulation

bus_inputs_1 = list(
  bus_line = shiny_input_bus_1_route,
  bus_num = floor(simul_time/shiny_input_bus_1_headway), #calcs bus num/ids from headway and simul period
  bus_arrvl_sd = shiny_input_bus_1_headway_sd,
  bus_arrvl_shdl = shiny_input_bus_1_headway,
  bus_pass_empty_num = floor(shiny_input_bus_1_capacity*shiny_input_bus_1_spare),
  bus_pass_empty_sd = 0, #zero variance in empty
  bus_pass_alight_num = 0, #zero passengers alighting
  bus_pass_alight_sd = 0, 
  bus_exit_condition = shiny_input_bus_exit_condition #zero variance in passengers alighting
  ) 

pass_inputs_1 = list(pass_num = shiny_input_pass_1_num,
                   pass_line = shiny_input_bus_1_route,
                   pass_headway_sd = shiny_input_pass_headway_sd, 
                   pass_headway = simul_time/shiny_input_pass_1_num*60, #convert to seconds  
                   pass_board = shiny_input_pass_board,
                   pass_board_sd = shiny_input_pass_board_sd)

#route 2 inputs=================================================================

#inputs from gui - bus
shiny_input_bus_2_route = "3L" #in minutes
shiny_input_bus_2_headway = 7 #in minutes
shiny_input_bus_2_headway_sd = 1 #in minutes
shiny_input_bus_2_capacity = 30 #in persons
shiny_input_bus_2_spare = .57 #in percent

#inputs from gui - pass
shiny_input_pass_2_num = 600 #total serviced for simulation

bus_inputs_2 = list(
  bus_line = shiny_input_bus_2_route,
  bus_num = floor(simul_time/shiny_input_bus_2_headway), #calcs bus num/ids from headway and simul period
  bus_arrvl_sd = shiny_input_bus_2_headway_sd,
  bus_arrvl_shdl = shiny_input_bus_2_headway,
  bus_pass_empty_num = floor(shiny_input_bus_2_capacity*shiny_input_bus_2_spare),
  bus_pass_empty_sd = 0, #zero variance in empty
  bus_pass_alight_num = 0, #zero passengers alighting
  bus_pass_alight_sd = 0,
  bus_exit_condition = shiny_input_bus_exit_condition, #zero variance in passengers alighting
  veh_flow = shiny_input_veh_flow,
  sat_flow = shiny_input_sat_flow) 

pass_inputs_2 = list(pass_num = shiny_input_pass_2_num,
                     pass_line = shiny_input_bus_2_route,
                     pass_headway_sd = shiny_input_pass_headway_sd, 
                     pass_headway = simul_time/shiny_input_pass_2_num*60, #convert to seconds   
                     pass_board = shiny_input_pass_board,
                     pass_board_sd = shiny_input_pass_board_sd)

#to function objects============================================================

#these items are what will be fed to the functions
bus_list = list(bus_inputs_1, bus_inputs_2)
pass_list = list(pass_inputs_1, pass_inputs_2)

xtra_delay_list = list(4, 
                       shiny_input_veh_flow, 
                       shiny_input_sat_flow)

xtra_delay_list = list(3, 
                       shiny_input_signal_cycle, 
                       shiny_input_signal_r_ratio)

#function output================================================================
#for singular simulation
bus_capcity_statisitcs = busCapacityCalculate(bus_list, pass_list, xtra_delay_list)[[1]]
df_bus = bus_capcity_statisitcs[[1]]
df_pass = bus_capcity_statisitcs[[2]]
df_left = bus_capcity_statisitcs[[3]]
df_remain = bus_capcity_statisitcs[[4]]

#defines columns/functions for summary statistics
names_glue = "{.col}.{.fn}"
list_fiveNum_cols = c("bus_queue_delay", "bus_board_dwell", "bus_exit_dwell", "bus_total_dwell")
list_fiveNum_func = list(mean = mean, median = median, sd = sd, high = max)

list_count_cols = c("bus_delayed_by_queue")
list_count_func = list(sum = sum)

df_left %>%  
  select(pass_line, starts_with("bus")) %>%  
  unique() %>% 
  # group_by(pass_line) %>% #this could be an input since the function 
  summarise(across(all_of(list_fiveNum_cols), list_fiveNum_func, .names = names_glue),
            across(all_of(list_count_cols), list_count_func, .names = names_glue)) %>% 
  ungroup() %>% 
  pivot_longer(cols = starts_with("bus_")) %>%  
  tidyr::separate(col = name, into = c("metric", "type"), sep = "\\.") %>%  
  pivot_wider(names_from = type, values_from = value)



furrr::future_pmap()
future::plan("multicore")
parallel::detectCores(logical = FALSE)
parallel::detectCores(logical = TRUE)
library(doParallel)
makepo
# Create a cluster object and then register: 
cl <- makePSOCKcluster(2)
registerDoParallel(cl)
stopCluster(cl)
#for n simulation
simul_number = 50 #user input 

bus_capcity_statisitcs = list(1:simul_number) %>%  
  pmap(function(m) busCapacityCalculate(bus_list, pass_list, xtra_delay_list))

#gets bus/passenger sort dataframe and makes some quick summary stats
simulation_left = list(bus_capcity_statisitcs, 1:simul_number) %>% 
  pmap(function(x, y) x[[1]][[3]] %>%  
         mutate(simulation = y[[1]])) %>% 
  reduce(bind_rows) %>% 
  mutate(bus_delayed_by_queue = case_when(bus_queue_delay == 0~0,
                                          bus_queue_delay != 0~1)) 
  

simulation_left$bus_queue_delay %>%  
  mean()

simulation_left_bus = simulation_left %>%  
  select(starts_with("bus_"), simulation) %>%  
  unique() 

simulation_left_bus_stats = simulation_left_bus %>% 
  group_by(simulation) %>%  
  summarise(across(c(bus_delayed_by_queue), sum, .names = "{.col}.{.fn}")) %>% 
  ungroup() %>% 
  summarise(mean = mean(bus_delayed_by_queue.1))

make_floor = function(data, column, num){
  data %>%  
    mutate(floor = (!!as.symbol(column) %/% num)*num)
}

simulation_left_bus %>%  
  select(simulation, bus_arrvl, bus_queue_delay, bus_board_start, bus_board_dwell, bus_exit) %>%  
  arrange(simulation , bus_arrvl)


simulation_left %>%  
  select(starts_with("bus_"), simulation) %>%  
  unique() %>% 
  summarise(mean = mean(bus_queue_delay)) 

#have to determine what stats you want to pull out and if you want to plot anything
  
  map(., ~.x %>% 
        select(starts_with("bus_")) %>%  
        unique() %>%  
        mutate(bus_delayed_by_queue = case_when(bus_queue_delay == 0~0,
                                                bus_queue_delay != 0~1)) %>%  
        summarise(across(c(bus_queue_delay, bus_board_dwell), list(mean = mean, sd = sd), .names = "{.col}.{.fn}"),
                  across(c(bus_delayed_by_queue), sum, .names = "{.col}.{.fn}"), 
                  total = n())) %>%
  reduce(bind_rows)

total %>%  
  mutate(simul_id = paste0("simulation_", 1:simul_number)) %>%  
  pivot_longer(cols = !simul_id, names_to = "metric") %>%  
  ggplot() + 
  geom_boxplot(aes(value)) +
  facet_grid(cols = vars(metric), scales = "free") +
  coord_flip() 
  
  



  data.frame(
    names = names(RVlist),
    values = unlist(RVlist, use.names = FALSE)
  )
  
  names(RVlist)[str_detect(names(RVlist) , "bus_route")]
           