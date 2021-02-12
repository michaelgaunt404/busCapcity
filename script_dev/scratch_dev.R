

make_bus_delay(xtra_delay_list, bus_delay_entry, bus_service_time)

###note::::::make extra delay list

observe({
  print(pass_inputs())
  rv_df_bus <<- df_bus()
  rv_RVlist <<- RVlist()
  rv_pass_inputs <<- pass_inputs()
})



rv_df_bus <<- df_bus()
rv_df_pass <<- df_pass()
rv_exit <<- exit_condition_inputs()
rv_RVlist <<- RVlist()
rv_pass_inputs <<- pass_inputs()
sim <<- simulation_results()

names(rvList) %>%  
  sort()

rvList = rv_RVlist
num_of_buses = 3
pass_input_list = get_list_items(rvList, string = "simul_duration|pass_board", purrr = F)
xtra_delay_list = rv_exit
input_list = pass_input_list
input = list(simul_num_berths = 1)
input$simul_num_berths

df_bus = get_bus_inputs(rvList, num_of_buses, pass_input_list)
df_pass = get_pass_inputs(rvList, num_of_buses, pass_input_list)

busCapacityCalculate(df_bus, df_pass, xtra_delay_list)[[1]][[3]] %>%
  select(starts_with("bus_")) %>%  unique()
  select(bus_line_id, i, bus_bay_assign, bus_arrvl_actl, bus_delay_entry, bus_board_start, bus_service_stop, bus_delay_exit, bus_exit_actual ) %>%  unique()



yolo = busCapacityCalculate(df_bus, df_pass, xtra_delay_list)

yolo[[1]][[3]] %>%  
  select(bus_line_id, i, bus_bay_assign, bus_arrvl_actl, bus_delay_entry, bus_board_start, bus_service_stop, bus_delay_exit, bus_exit_actual ) %>%  unique()



busCapacityCalculate = function(df_bus, df_pass, xtra_delay_list){
  #need try_catch error and messages
  #need default numbers 
  #what is behavior for bus without a full load - does it wait some amount of time?
  
  # quick initialization
  # i = 0
  # i %+=% 1
  left_list = list()
  already_left = 0
  current_queue_penalty = 0 #needs to account for exit delay but now only consists of pass_board_total
  current_open_bay = 1
  bus_exit_actual = 0
  
  for (i in 1:length(df_bus$bus_line_id) ){
    print(i)
    bus_board_start = (df_bus[i, "bus_arrvl_actl"] + current_queue_penalty) 
    
    left_list[[i]] = df_pass %>%  
      filter(pass_id %not_in% already_left, 
             pass_arrvl < bus_board_start, 
             bus_line == df_bus[i, "bus_line"]) %>% 
      .[1:df_bus[i, "bus_surplus_seats"], ] %>% #this pipe should end hear and and become its own object to include a wait period - maybe %>%  
      # na.omit() %>% #need to filter NA rows - occurs if theres less ppl at stop than bus surplus
      mutate(bus_line_id = paste0(df_bus[i, "bus_line"], "_", df_bus[i, "bus_id"])) %>% 
      merge.data.frame(., df_bus %>% 
                         select(bus_id, bus_arrvl_actl, bus_pass_empty, bus_pass_alight, bus_service_time_alight, bus_surplus_seats, bus_line_id, bus_route_door_cond), 
                       by= "bus_line_id", all.x = T) %>% 
      mutate(bus_line = bus_line_id %>%  
               gsub('(.*)_\\w+', '\\1', .),
             i = i,
             bus_board_start = bus_board_start,
             bus_bay_assign = current_open_bay, 
             bus_delay_entry = bus_board_start-df_bus[i, "bus_arrvl_actl"],
             bus_service_time_board = sum(pass_board, na.rm = T ),
             bus_service_time = case_when(bus_route_door_cond == "Series" ~ bus_service_time_alight+bus_service_time_board, 
                                          T~max(bus_service_time_alight,bus_service_time_board)),
             bus_service_stop = bus_board_start + bus_service_time, #what is longer alight or board
             bus_delay_exit = case_when(bus_service_stop < bus_exit_actual ~ bus_exit_actual-bus_service_stop, 
                                        T~make_bus_delay(xtra_delay_list, bus_delay_entry, bus_service_time)),
             bus_time_inBerth = bus_delay_exit + bus_service_time,
             bus_exit_actual = bus_board_start + bus_time_inBerth,
             bus_delayed_by_queue = case_when(bus_delay_entry == 0~0,
                                              bus_delay_entry != 0~1),
             bus_pass_picked_up = bus_surplus_seats-sum(is.na(pass_id)),
             bus_delay_total = bus_delay_entry + bus_delay_exit,
             bus_no_pick_up = case_when(bus_pass_picked_up == 0~0,
                                        bus_pass_picked_up != 0~1)
      ) 
    
    bus_exit_actual = left_list[[i]]$bus_exit_actual[1]
    bus_interference_check = (bus_exit_actual < df_bus[i+1, "bus_arrvl_actl"])
    bus_interference_check = ifelse(is.na(bus_interference_check), T, bus_interference_check)
    
    if (bus_interference_check){
      #no interference always reset nerth assignment to 1
      current_open_bay = 1
      current_queue_penalty = 0
    } else {
      if (current_open_bay == input$simul_num_berths ) {
        #if at last berth, assign to first with time penalty
        current_open_bay = 1
        current_queue_penalty = (bus_exit_actual - df_bus[i+1, "bus_arrvl_actl"])
      } else {
        current_open_bay %+=% 1
        current_queue_penalty = 0
      }
    }
    
    already_left = c(already_left, 
                     left_list[[i]] %>%
                       na.omit() %>% 
                       .[["pass_id"]])
  }
  
  #reduces all list elements to single dataframe with passengers sorted to buses 
  df_left = left_list  %>%  
    reduce(bind_rows) %>% 
    mutate(pass_wait = bus_board_start-pass_arrvl)  
  
  #returns passengers who are left
  df_remain = df_pass %>%  
    filter(pass_id %not_in% df_left$pass_id)
  
  return(list(list(df_bus, df_pass, df_left, df_remain)))
}


