#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is a utility script that holds custom functions 
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: script defines custom functions
#-------- script defines custom functions
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# tmp = left_list[[i]]
# ask_time = 187
# tmp %>%  
#   mutate(yolo = extra_delay_signal(bus_queue_delay, bus_board_dwell, input_list_signal))

# shiny_input_signal_cycle = 60  #in sec
# shiny_input_signal_r_ratio = .4
# input_list_signal = list(shiny_input_bus_exit_condition, 
#                          shiny_input_signal_cycle, 
#                          shiny_input_signal_r_ratio)

#exit delay functions===========================================================
#all of these need one list input that will be indexed properly
extra_delay_signal = function(col_q_dly, col_brd_dwll, input_list_signal){
  ask_time = col_q_dly + col_brd_dwll #queue and board delay
  cycle = input_list_signal[[2]]
  r_ratio = input_list_signal[[3]]
  c_ratio = (ask_time/cycle)-floor(ask_time/cycle)
  case_when(c_ratio<r_ratio ~ (r_ratio-c_ratio)*cycle, 
            c_ratio>=r_ratio ~ 0)
}

#makes extra bus headway - needs to be built to account for other methods
extra_delay_gaps = function(input_list_flow){
  gamma = input_list_flow[[2]]/3600 #veh/hr of effective green 
  tau = 3600/input_list_flow[[3]] #saturation headway?
  theta = tau*gamma #capcity vol/cap ratio?
  
  data.frame(u = runif(100), 
             theta = gamma/tau) %>%
    mutate(rho = case_when(u<theta~theta, 
                           u>=theta~u), 
           headway = tau-(log((1-rho)/(1-theta))/gamma), 
           headway_total = cumsum(headway),
           flag = case_when(headway<tau~"N", 
                            headway>tau~"Y")) %>%  
    filter(flag == "Y") %>%  
    .[1, "headway_total"]
}

#makes dist the length of input
quick_dist = function(num, sd, floor = F){
  if (sd == 0){
  rep(sd, num)
  } else {
    if (!floor) {
      rnorm(num, mean = 0, sd = sd) %>% 
        dgt2() 
    } else {
      rnorm(num, mean = 0, sd = sd) %>% 
        dgt2() %>% 
        round(0)
    }
  }
}

#df creation====================================================================
#create dataframe for a singular bus route
create_bus = function(bus) {
  data.frame(bus_id = seq(1:bus$bus_num), 
             bus_line = bus$bus_line,
             bus_arrvl = cumsum(bus$bus_arrvl_shdl+quick_dist(bus$bus_num, bus$bus_arrvl_sd))*60, #cummaltive sum of bus arrivals with sd converted to seconds
             bus_pass_empty = (bus$bus_pass_empty_num + quick_dist(bus$bus_num, bus$bus_pass_empty_sd, T)) %>% #number of empty seats per bus with sd
               lmt0(),
             bus_pass_alight = (bus$bus_pass_alight_num + quick_dist(bus$bus_num, bus$bus_pass_alight_sd, T)) %>% #number of people leaving bus with sd
               lmt0(), 
             bus_obstrct_time = 0 #seconds = place holder for conditional extra delays
             ) %>%  
    mutate(bus_line_id = paste0(bus_line, "_", bus_id), 
           bus_surplus_seats = bus_pass_empty+bus_pass_alight) #calcualtes total seat surplus for a bus
}


#creates dataframe with all buses and their meta-data
create_buses = function(bus_list){
  bus_list %>%  
    map(create_bus) %>%  
    reduce(bind_rows) %>%  
    arrange(bus_arrvl)
}

#create dataframe for a singular bus route's passengers
create_pass = function(pass) {
  data.frame(pass_id = seq(1:pass$pass_num),
             pass_line = pass$pass_line, 
             pass_arrvl = cumsum(pass$pass_headway+quick_dist(pass$pass_num, pass$pass_headway_sd, T)),
             pass_board = dgt2(pass$pass_board+quick_dist(pass$pass_num, pass$pass_board_sd))
             ) %>% 
    mutate(pass_arrvl = case_when(pass_arrvl <= 0~0, 
                                  pass_arrvl > 0~pass_arrvl)) %>% 
    arrange(pass_arrvl)
}

#creates dataframe with all bus routes' passengers and their meta-data
create_people = function(pass_list){
  pass_list %>%  
    map(create_pass) %>%  
    reduce(bind_rows) %>%  
    arrange(pass_arrvl)
}

# make_bus_delay = function(xtra_delay_list, col_q_dly, col_brd_dwll){
#   case_when(xtra_delay_list[[1]]==1~0,
#             xtra_delay_list[[1]]==2~0,
#             xtra_delay_list[[1]]==3~extra_delay_signal(col_q_dly, col_brd_dwll, xtra_delay_list), #take extra_delay_signal function
#             xtra_delay_list[[1]]==4~extra_delay_gaps(xtra_delay_list)
#   )
# }


#main function==================================================================
#this function perfroms four operations 
#it creates both bus and passneger dataframes 
#it perfroms the bus and passenger sorting 
#it calcualtes who was not picked up by a bus
#it returns a list of these dataframes
busCapacityCalculate = function(bus_list, pass_list, xtra_delay_list){
  #need try_catch error and messages
  #need default numbers 
  #what is behavior for bus without a full load - does it wait some amount of time?
  
  df_bus = create_buses(bus_list)
  
  df_pass = create_people(pass_list)
  
  # quick initialization
  # i %+=% 1
  left_list = list()
  already_left = 0
  current_queue_penalty = 0 #needs to account for exit delay but now only consists of pass_board_total
  
  for (i in 1:length(df_bus$bus_id)){
    
    #bus_board_start => bus_service_start
    bus_board_start = (df_bus[i, "bus_arrvl"] + current_queue_penalty) 
    
    left_list[[i]] = df_pass %>%  
      filter(pass_id %not_in% already_left, 
             pass_arrvl < bus_board_start, 
             pass_line == df_bus[i, "bus_line"]) %>% 
      .[1:df_bus[i, "bus_surplus_seats"], ] %>% #this pipe should end hear and and become its own object to include a wait period - maybe %>%  
      na.omit() %>% #need to filter NA rows - occurs if theres less ppl at stop than bus surplus
      mutate(bus_line_id = paste0(df_bus[i, "bus_line"], "_", df_bus[i, "bus_id"])) %>% 
      merge.data.frame(., df_bus %>% 
                         select(bus_id, bus_arrvl, bus_surplus_seats, bus_line_id), 
                       by= "bus_line_id", all.x = T) %>% 
      mutate(bus_board_start = bus_board_start, 
             
             #bus_queue_delay => bus_delay_entry
             bus_queue_delay = bus_board_start-df_bus[i, "bus_arrvl"], 
             
             #bus_board_dwell => bus_service_time - need door and start/stop stuff
             bus_board_dwell = sum(pass_board), #service time with door operations - might have to make this a function based on different door scenerios
             
             #bus_board_start => bus_service_stop
             bus_exit = bus_board_start + bus_board_dwell, #what is longer alight or board
             
             #bus_non_service_dwell - for non-operational buses that chill at a stop - may not end up here 
             
             #bus_exit_dwell => bus_delay_exit
             bus_exit_dwell = make_bus_delay(xtra_delay_list, bus_queue_delay, bus_board_dwell), 
             
             #bus_total_dwell => bus_time_inBerth
             bus_total_dwell = bus_exit_dwell + bus_board_dwell,
             bus_exit_actual = bus_board_start + bus_total_dwell, 
             
             #bus_delayed_by_queue => bus_in_queue
             bus_delayed_by_queue = case_when(bus_queue_delay == 0~0,
                                          bus_queue_delay != 0~1)) 
    #put something 
  
    bus_exit_actual = left_list[[i]]$bus_exit_actual[1]
    bus_interference_check = bus_exit_actual < df_bus[i+1, "bus_arrvl"]
    current_queue_penalty = ifelse(bus_interference_check, 
                                   0, 
                                   (bus_exit_actual - df_bus[i+1, "bus_arrvl"]))
    
    already_left = c(already_left, left_list[[i]][["pass_id"]])
  }
  
  #reduces all list elements to single dataframe with passengers sorted to buses 
  df_left = left_list  %>%  
    reduce(bind_rows) %>% 
    arrange(pass_arrvl) %>%  
    mutate(pass_wait = bus_arrvl-pass_arrvl) %>%  
    group_by(bus_line_id) %>% 
    mutate(pass_pick_up = n(), 
           bus_seats_left_empty = bus_surplus_seats-pass_pick_up) %>%  
    ungroup()
  
  #returns passengers who are left
  df_remain = df_pass %>%  
    filter(pass_id %not_in% df_left$pass_id)
  
  return(list(list(df_bus, df_pass, df_left, df_remain)))
}


