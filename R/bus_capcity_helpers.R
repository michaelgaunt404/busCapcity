#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is a utility script that holds custom functions w.r.t bus capacity modeling 
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: script defines custom functions
#-------- script defines custom functions
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#exit delay functions===========================================================
#all of these need one list input that will be indexed properly
extra_delay_signal = function(col_q_dly, col_brd_dwll, input_list_signal){
  ask_time = col_q_dly + col_brd_dwll #queue and board delay
  cycle = input_list_signal[["exit_cond_status_cycle_time"]]
  r_ratio = input_list_signal[["exit_cond_red_ratio"]]
  c_ratio = (ask_time/cycle)-floor(ask_time/cycle)
  case_when(c_ratio<r_ratio ~ (r_ratio-c_ratio)*cycle, 
            c_ratio>=r_ratio ~ 0)
}

#makes extra bus headway - needs to be built to account for other methods
extra_delay_gaps = function(input_list_flow){
  gamma = input_list_flow[["exit_cond_veh_flow"]]/3600 #veh/hr of effective green 
  tau = 3600/input_list_flow[["exit_cond_sat_flow"]] #saturation headway?
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

#creates big list for xtra_delays
make_bus_delay = function(xtra_delay_list, col_q_dly, col_brd_dwll){
  case_when(xtra_delay_list[["exit_cond_status"]] == "1" ~ 0,
            xtra_delay_list[["exit_cond_status"]] == "2" ~ 0,
            xtra_delay_list[["exit_cond_status"]] == "3" ~ extra_delay_signal(col_q_dly, col_brd_dwll, xtra_delay_list), #take extra_delay_signal function
            xtra_delay_list[["exit_cond_status"]] == "4" ~ extra_delay_gaps(xtra_delay_list)
  )
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
             bus_arrvl_schl = bus$bus_route_headway,
             bus_pass_empty = (bus$bus_pass_empty_num + quick_dist(bus$bus_num, bus$bus_pass_empty_sd, T)) %>% #number of empty seats per bus with sd
               lmt0(),
             bus_pass_alight = (bus$bus_pass_alight_num + quick_dist(bus$bus_num, bus$bus_pass_alight_sd, T)) %>% #number of people leaving bus with sd
               lmt0(), 
             bus_obstrct_time = 0 #seconds = place holder for conditional extra delays
             ) %>%  
    mutate(
           bus_surplus_seats = bus_pass_empty+bus_pass_alight,
           bus_arrvl_schl = cumsum(bus_arrvl_schl)*60,
           bus_arrvl_actl = bus_arrvl_schl+quick_dist(bus$bus_num, bus$bus_route_headway_sd)*60
           ) 
}

#creates dataframe with all buses and their meta-data
create_buses = function(bus_list){
  bus_list %>%
    map(create_bus) %>%
    reduce(bind_rows) %>%
    arrange(bus_arrvl)
}

#create dataframe for a singular bus route's passengers
create_pass = function(pass, simul_time){
  tmp_num_pass = round(pass$bus_route_pass*(simul_time/60))
  
  data.frame(pass_id = seq(1: tmp_num_pass),
             pass_arrvl = pass$pass_headway,
             pass_board = dgt2(pass$pass_board + quick_dist(tmp_num_pass, pass$pass_board_sd))
  ) %>%
    mutate(pass_arrvl = cumsum(pass_arrvl) + quick_dist(tmp_num_pass, pass$bus_route_pass_sd, T),
           pass_arrvl = case_when(pass_arrvl <= 0~0,
                                  pass_arrvl > 0~pass_arrvl)) %>%
    arrange(pass_arrvl)
}

# creates dataframe with all bus routes' passengers and their meta-data
create_people = function(pass_list){
  pass_list %>%
    map(create_pass) %>%
    reduce(bind_rows) %>%
    arrange(pass_arrvl)
}

get_bus_inputs =  function(rvList, num_of_buses, simul_time){
  #perfroms a few steps 
  #->grabs all "bus_" inupts and groups by"_#" suffix
  #->first map removes suffixs
  #->mutate sec. creates inputs per bus route
  #->"group_by()" and down makes buses using purrr functionality
  
  #NOTE: have to change third input to list with time and other inputs (CONDITION)
  
  list(1:num_of_buses) %>%  
    pmap(function(x) 
      get_list_items(rvList, string = "bus_", suffix = x)
      # rvList[names(rvList)[(str_detect(names(rvList) , paste0("_", x))+str_detect(names(rvList) , "bus_"))==2]]
    ) %>% 
    map(
      ~{names(.)<- names(.) %>%  str_sub(end = -3);.}
    ) %>% 
    map_dfr(bind_rows) %>%  
    mutate(bus_num = floor(simul_time/bus_route_headway),
           bus_pass_empty_num = floor(as.numeric(bus_route_size)*(bus_route_cap/100)),
           bus_pass_empty_sd = 0, #zero variance in empty
           bus_pass_alight_num = 0, #zero passengers alighting
           bus_pass_alight_sd = 0,
           bus_exit_condition = 2) %>%
    group_by(bus_line) %>%
    nest(cols = !bus_line) %>%
    mutate(create_bus = map(cols, create_bus)) %>%
    unnest(everything()) %>%  
    mutate(bus_line_id = paste0(bus_line, "_", bus_id)) %>% 
    arrange(bus_arrvl_actl) %>% 
    as.data.frame() 
  
}

get_pass_inputs =  function(rvList, num_of_buses, pass_input_list){
  list(1:num_of_buses) %>%
    pmap(function(x)
      get_list_items(rvList, string = "pass_|bus_line_", suffix = x)
    ) %>%
    map(
      ~{names(.)<- names(.) %>%  str_sub(end = -3);.} #removes suffixs from input names
    ) %>%
    map_dfr(bind_rows) %>% #combines to single df
    mutate(
      pass_headway = 1/(bus_route_pass/3600),
      pass_board = pass_input_list[[2]],
      pass_board_sd = pass_input_list[[3]]
    ) %>%
    group_by(bus_line) %>% #perfroms old "create_buses()" function
    nest(cols = !bus_line) %>%
    mutate(create_pass = map(cols, function(x)  create_pass(x, pass_input_list[[1]]))) %>%
    unnest(cols = "create_pass") %>%
    arrange(pass_arrvl) %>%  
    select(!cols) %>% 
    as.data.frame() 
}

busCapacityCalculate = function(df_bus, df_pass, xtra_delay_list){
  #need try_catch error and messages
  #need default numbers 
  #what is behavior for bus without a full load - does it wait some amount of time?
  
  # quick initialization
  i = 0
  i %+=% 1
  left_list = list()
  already_left = 0
  current_queue_penalty = 0 #needs to account for exit delay but now only consists of pass_board_total
  
  for (i in 1:length(df_bus$bus_line_id) ){
    bus_board_start = (df_bus[i, "bus_arrvl_actl"] + current_queue_penalty) 
    
    left_list[[i]] = df_pass %>%  
      filter(pass_id %not_in% already_left, 
             pass_arrvl < bus_board_start, 
             bus_line == df_bus[i, "bus_line"]) %>% 
      .[1:df_bus[i, "bus_surplus_seats"], ] %>% #this pipe should end hear and and become its own object to include a wait period - maybe %>%  
      # na.omit() %>% #need to filter NA rows - occurs if theres less ppl at stop than bus surplus
      mutate(bus_line_id = paste0(df_bus[i, "bus_line"], "_", df_bus[i, "bus_id"])) %>% 
      merge.data.frame(., df_bus %>% 
                         select(bus_id, bus_arrvl_actl, bus_surplus_seats, bus_line_id), 
                       by= "bus_line_id", all.x = T) %>% 
      mutate(bus_line = bus_line_id %>%  
               gsub('(.*)_\\w+', '\\1', .),
             bus_board_start = bus_board_start,
             bus_delay_entry = bus_board_start-df_bus[i, "bus_arrvl_actl"],
             bus_service_time = sum(pass_board, na.rm = T ), #service time with door operations - might have to make this a function based on different door scenerios
             bus_service_stop = bus_board_start + bus_service_time, #what is longer alight or board
             
             #non-service dwell time 
             
             bus_delay_exit = make_bus_delay(xtra_delay_list, bus_delay_entry, bus_service_time),
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
    bus_interference_check = bus_exit_actual < df_bus[i+1, "bus_arrvl_actl"]
    current_queue_penalty = ifelse(bus_interference_check, 
                                   0, 
                                   (bus_exit_actual - df_bus[i+1, "bus_arrvl_actl"]))
    
    already_left = c(already_left, 
                     left_list[[i]] %>%
                       na.omit() %>% 
                       .[["pass_id"]])
  }
  
  #reduces all list elements to single dataframe with passengers sorted to buses 
  df_left = left_list  %>%  
    reduce(bind_rows) %>% 
    # arrange(pass_arrvl) %>%  
    mutate(pass_wait = bus_board_start-pass_arrvl)  
    # group_by(bus_line_id) %>% 
    # mutate(pass_pick_up = n(), 
    #        bus_seats_left_empty = bus_surplus_seats-pass_pick_up) %>%  
    # ungroup()
  
  #returns passengers who are left
  df_remain = df_pass %>%  
    filter(pass_id %not_in% df_left$pass_id)
  
  return(list(list(df_bus, df_pass, df_left, df_remain)))
}

#get metrics from simulation object=============================================
#defines columns/functions for summary statistics
names_glue = "{.col}.{.fn}"
list_fiveNum_cols = c("bus_service_time", "bus_delay_entry", "bus_delay_exit", "bus_delay_total", "bus_pass_picked_up")
list_fiveNum_func = list(mean = mean, median = median, sd = sd, high = max)

list_count_cols = c("bus_delayed_by_queue", "bus_no_pick_up")
list_count_func = list(sum = sum)

#gets summary statisitcs
get_summry_statistics = function(data, grouped = F, group){
  
  if (!grouped){
    tmp_data = data %>%  
      select(bus_line, bus_line_id, starts_with("bus")) %>%
      unique() 
    
  } else {
    tmp_data = data %>%  
      select(bus_line, bus_line_id, starts_with("bus")) %>%
      unique() %>% 
      group_by(!!as.symbol( group)) 
  }
  
  tmp_data %>%
    summarise(across(all_of(list_fiveNum_cols), list_fiveNum_func, .names = names_glue),
              across(all_of(list_count_cols), list_count_func, .names = names_glue)) %>% 
    pivot_longer(cols = !starts_with("bus_line")) %>%  
    tidyr::separate(col = name, into = c("metric", "type"), sep = "\\.") %>%  
    mutate(across(value, round, 1)) %>% 
    pivot_wider(names_from = type, values_from = value)
  
}

