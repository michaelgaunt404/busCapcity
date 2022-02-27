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
  case_when(xtra_delay_list[["exit_cond_status"]] == "1" ~ 0
            ,xtra_delay_list[["exit_cond_status"]] == "2" ~ 0
            ,xtra_delay_list[["exit_cond_status"]] == "3" ~ extra_delay_signal(col_q_dly, col_brd_dwll, xtra_delay_list) #add extra_delay_signal function
            ,xtra_delay_list[["exit_cond_status"]] == "4" ~ extra_delay_gaps(xtra_delay_list) #add extra_delay_signal function
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

make_density = function(mean, sd, lmt = F){
  if (lmt){
  (mean+abs(rnorm(n = 1e6, mean = 0, sd = sd))) %>% 
    density() %>% 
    plot(main = str_glue("Mean: {mean} & Standard Deviation: {sd}"))
  } else {
    (mean+(rnorm(n = 1e6, mean = 0, sd = sd))) %>% 
      density() %>% 
      plot(main = str_glue("Mean: {mean} & Standard Deviation: {sd}"))
  }
}

make_histogram = function(mean, sd, lmt = F){
  (mean+abs(rnorm(n = 1e6, mean = 0, sd = sd))) %>% 
    hist(main = str_glue("Mean: {mean} & Standard Deviation: {sd}"))
}

#df creation====================================================================
#create dataframe for a singular bus route

create_bus = function(bus) {
  #run for debug 
  # bus = tmpp %>% filter(bus_line == "bus_route_1")
  
  # tmppp = 
  data.frame(bus_id = seq(1:bus$bus_num), 
             bus_arrvl_schl = bus$bus_route_headway,
             bus_pass_empty = (bus$bus_pass_empty_num + quick_dist(bus$bus_num, bus$bus_pass_empty_sd, T)   ) %>% #number of empty seats per bus with sd
               lmt0(),
             bus_pass_alight = dgt0(
               bus$bus_pass_onboard_num * lmt_between(rnorm(bus$bus_num, bus$bus_route_num_alight, bus$bus_route_num_alight_sd))
             ),
             bus_obstrct_time = 0) %>%  #seconds = place holder for conditional extra delays
    mutate(bus_service_time_alight = map(bus_pass_alight, ~lmt0(   rnorm(.x, bus$pass_board, bus$pass_board_sd)   ) %>%  
                                           sum()) %>%  
             unlist()) %>% 
    mutate(
      bus_surplus_seats = bus_pass_empty+bus_pass_alight,
      bus_arrvl_schl = cumsum(bus_arrvl_schl)*60,
      bus_arrvl_actl = bus_arrvl_schl+quick_dist(bus$bus_num, bus$bus_route_headway_sd)*60
    ) 
}

get_bus_inputs =  function(rvList, num_of_buses, input_list){
  #run for debug 
  # rvList = rv_RVlist; num_of_buses = rv_RVlist$simul_num_routes; input_list = rv_pass_inputs;
  #perfroms a few steps 
  #->grabs all "bus_" inupts and groups by"_#" suffix
  #->first map removes suffixs
  #->mutate sec. creates inputs per bus route
  #->"group_by()" and down makes buses using purrr functionality
  # tmpp = 
  list(1:num_of_buses) %>%  
    pmap(function(x) 
      get_list_items(rvList, string = "bus_", suffix = x) %>%  
        data.frame() 
    ) %>% 
    map(
      ~{names(.) <- names(.) %>%
        str_sub(end = -3);.}
    ) %>% 
    map(~.x %>% 
          select('bus_route_cap',
                 'bus_route_headway_sd',
                 'bus_route_headway',
                 'bus_route_pass_sd',
                 'bus_route_door_cond',
                 'bus_route_num_alight_sd',
                 'bus_route_pass',
                 'bus_size',
                 'bus_route_num_alight',
                 'bus_line',
                 'bus_route_size'
          ) %>%  
          mutate_if(is.integer, as.numeric)
    ) %>% 
    reduce(bind_rows) %>% 
    mutate(bus_num = floor(input_list[["simul_duration"]]/bus_route_headway),
           bus_pass_empty_num = floor(as.numeric(bus_route_size)*(bus_route_cap/100)),
           bus_pass_onboard_num = bus_route_size-bus_pass_empty_num,
           bus_pass_empty_sd = 0, #zero variance in empty
           # bus_pass_alight_num = floor(bus_pass_onboard_num*(bus_route_num_alight /100)), #MG20211221_i think these are redundant - this needs to be calculated on a per bus basis in create_bus() 
           # bus_pass_alight_sd = floor(bus_pass_onboard_num*(bus_route_num_alight_sd /100)), #MG20211221_i think these are redundant - this needs to be calculated on a per bus basis in create_bus()
           bus_route_num_alight = bus_route_num_alight/100, #but do convert to percents as they should be 
           bus_route_num_alight_sd = bus_route_num_alight_sd/100, #but do convert to percents as they should be 
           pass_board = input_list[["pass_board"]],
           pass_board_sd = input_list[["pass_board_sd"]],
           bus_exit_condition = 2) %>%
    group_by(bus_line) %>%
    nest(cols = !bus_line) %>%
    mutate(create_bus = map(cols, create_bus)) %>%
    unnest(everything()) %>% 
    ungroup() %>% 
    mutate(bus_line_id = paste0(bus_line, "_", bus_id)) %>% 
    arrange(bus_arrvl_actl) 
  
}

#create dataframe for a singular bus route's passengers
create_pass = function(pass, simul_time){

  tmp_num_pass = round(pass$bus_route_pass*(simul_time/60))
  
  data.frame(pass_id = seq(1: tmp_num_pass),
             pass_arrvl = pass$pass_headway,
             pass_board = dgt2(pass$pass_board + abs(quick_dist(tmp_num_pass, pass$pass_board_sd)))
  ) %>%
    mutate(pass_arrvl = cumsum(pass_arrvl) + quick_dist(tmp_num_pass, pass$bus_route_pass_sd, T),
           pass_arrvl = case_when(pass_arrvl <= 0~0,
                                  pass_arrvl > 0~pass_arrvl)) %>%
    arrange(pass_arrvl)
}

# rvList = rv_RVlist
# num_of_buses = rv_RVlist$simul_num_routes
# input_list = rv_pass_inputs

get_pass_inputs =  function(rvList, num_of_buses, pass_input_list){
  list(1:num_of_buses) %>%
    pmap(function(x)
      get_list_items(rvList, string = "pass_|bus_line_", suffix = x) %>%  
        data.frame() 
    ) %>%
    map(
      ~{names(.) <- names(.) %>%
        str_sub(end = -3);.}
    ) %>% 
    map(~.x %>% 
          select("bus_route_pass_sd", "dist_route_pass", "bus_route_pass", "bus_line") %>%  
          mutate_if(is.integer, as.numeric)
    ) %>%  
    reduce(bind_rows) %>% 
    mutate(
      pass_headway = 1/(bus_route_pass/3600),
      pass_board = pass_input_list[["pass_board"]],
      pass_board_sd = pass_input_list[["pass_board_sd"]]
    ) %>%
    group_by(bus_line) %>% #perfroms old "create_buses()" function
    nest(cols = !bus_line) %>%
    mutate(create_pass = map(cols, function(x)  create_pass(x, pass_input_list[["simul_duration"]] ) ) ) %>%
    unnest(cols = "create_pass") %>%
    arrange(pass_arrvl) %>%  
    select(!cols) %>% 
    mutate(pass_id_line = str_glue("{bus_line}_{pass_id}")) %>% 
    as.data.frame() 
}

busCapacityCalculate = function(df_bus, df_pass, xtra_delay_list, berths){
  #run if trouble shooting
  # df_bus = tmp_raw[[1]][[1]]; df_pass = tmp_raw[[2]][[1]]; xtra_delay_list =  rv_exit; berths = 1;
  # df_bus = data[[1]]; df_pass = data[[2]]; xtra_delay_list =  rv_exit; berths = 3; #use with recent data
  # simulation = data[[3]]
  
  #need try_catch error and messages
  #need default numbers 
  #what is behavior for bus without a full load - does it wait some amount of time?
  
  print(paste0("Number of berths: ", berths))
  
  # quick initialization
  i = 0
  i %+=% 1
  left_list = list()
  already_left = 0
  current_queue_penalty = 0 #needs to account for exit delay but now only consists of pass_board_total
  current_open_bay = 1 #starts first bay
  bus_exit_actual_init = 0
  
  for (i in 1:length(df_bus$bus_line_id) ){
    print(i)
    print(paste0("check: ", (bus_exit_actual_init < df_bus[i+1, "bus_arrvl_actl"][[1]])))
    print(paste0("current_queue_penalty: ", current_queue_penalty))
    print(paste0("current_open_bay: ", current_open_bay))
    # bus_board_start = (df_bus[i, "bus_arrvl_actl"] + current_queue_penalty) #dont need to do this - i do it in df below
    
    #makes passenger relevant to bus[[i]]
    # rel_pass_list = df_pass %>%  
    #   filter(pass_id_line %not_in% already_left) %>% 
    #   filter(pass_arrvl < bus_board_start[[1]]) %>% #gets all passengers at stop before bus arrival 
    #   filter(bus_line == df_bus[[i, "bus_line"]]) %>% #gets only relevant passengers for given bys
    #   .[1:df_bus[i, "bus_surplus_seats"][[1]], ] #NA values difference beween people alighting and boarding
    # 
    # #augments above df to have bus attributes 
    # rel_pass_list_aug = rel_pass_list %>% 
    #   mutate(bus_line_id = paste0(df_bus[i, "bus_line"], "_", df_bus[i, "bus_id"])) %>% #assigns bus_line/id to passengers
    #   merge.data.frame(., df_bus %>% 
    #                      select(bus_id, bus_line_id, bus_arrvl_actl, bus_pass_empty, bus_pass_alight, bus_service_time_alight, bus_surplus_seats,  bus_route_door_cond), 
    #                    by = c("bus_line_id"), all.x = T)
    # 
    # 
    # #performs important calculations for simulations 
    # temp = rel_pass_list_aug %>% 
    #   mutate(bus_line = bus_line_id %>% gsub('(.*)_\\w+', '\\1', .)
    #          ,i = i
    #          ,bus_board_start = bus_board_start #adds when bus starts operation - includes entry penalty
    #          ,bus_bay_assign = current_open_bay #assigns bay to bus - created in previous loop or initialization
    #          ,bus_delay_entry = bus_board_start-df_bus[i, "bus_arrvl_actl"] #calculates entry delay from theoretical start
    #          ,bus_service_time_board = sum(pass_board, na.rm = T )
    #          ,bus_service_time = case_when(bus_route_door_cond == "Series" ~ bus_service_time_alight + bus_service_time_board, 
    #                                        T~max(bus_service_time_alight, bus_service_time_board)) #ttl service time based on door condition
    #          ,bus_service_stop = bus_board_start + bus_service_time) #determines when bus stops
    
    
    # temp %>%  
    #   mutate(bus_delay_exit_cond_1 = bus_exit_actual-bus_service_stop
    #          ,)
    # 
    # temp %>%  
    #   mutate(bus_delay_exit = case_when(temp$bus_service_stop[1] < bus_exit_actual ~ (bus_exit_actual-bus_service_stop), 
    #                                     ,T
    #                                     ,T & xtra_delay_list[["exit_cond_status"]] == "1" ~ 0
    #                                     ,T & xtra_delay_list[["exit_cond_status"]] == "2" ~ 0))
    #                                     ,T~make_bus_delay(xtra_delay_list, temp$bus_delay_entry[1], temp$bus_service_time[1]))
    #   )
    # 
    # 
    # 
    #   mutate(bus_delay_exit = case_when(bus_service_stop < bus_exit_actual ~ (bus_exit_actual-bus_service_stop), 
    #                                     T~make_bus_delay(xtra_delay_list, temp$bus_delay_entry[1], temp$bus_service_time[1]))
    #   )
    #          ,bus_time_inBerth = bus_delay_exit + bus_service_time
    #          ,bus_exit_actual = bus_board_start + bus_time_inBerth
    #          ,bus_delayed_by_queue = case_when(bus_delay_entry == 0~0,
    #                                            bus_delay_entry != 0~1)
    #          ,bus_pass_picked_up = bus_surplus_seats-sum(is.na(pass_id))
    #          ,bus_delay_total = bus_delay_entry + bus_delay_exit
    #          ,bus_total_operation = bus_delay_entry  + bus_service_time + bus_delay_exit
    #          ,bus_no_pick_up = case_when(bus_pass_picked_up == 0~0,
    #                                      bus_pass_picked_up != 0~1)
    #   )
    # temp %>%  colnames()
    # temp  %>%  
    #   mutate(yolo = make_bus_delay(xtra_delay_list, bus_delay_entry, bus_service_time))
    # 
    # make_bus_delay
    # identical(left_list[[i]], temp)
    
    left_list[[i]] = df_pass %>%  
      filter(pass_id_line %not_in% already_left) %>% 
      filter(pass_arrvl < bus_board_start[[1]]) %>% #gets all passengers at stop before bus arrival 
      filter(bus_line == df_bus[[i, "bus_line"]]) %>% #gets only relevant passengers for given bys
      .[1:df_bus[i, "bus_surplus_seats"][[1]], ] %>% #this pipe should end hear and and become its own object to include a wait period - maybe %>%  
      # na.omit() %>% #need to filter NA rows - occurs if theres less ppl at stop than bus surplus
      mutate(bus_line_id = paste0(df_bus[i, "bus_line"], "_", df_bus[i, "bus_id"])) %>% #assigns bus_line/id to passengers
      merge.data.frame(., df_bus %>% 
                         select(bus_id, bus_line_id, bus_arrvl_actl, bus_pass_empty, bus_pass_alight, bus_service_time_alight, bus_surplus_seats,  bus_route_door_cond), 
                       by = c("bus_line_id"), all.x = T) %>% #merges df_bus attributes to passenger list
      mutate(bus_line = bus_line_id %>% gsub('(.*)_\\w+', '\\1', .), 
             i = i,
             bus_board_start = bus_arrvl_actl + current_queue_penalty,
             bus_bay_assign = current_open_bay, 
             bus_delay_entry = current_queue_penalty,
             bus_service_time_board = sum(pass_board, na.rm = T ),
             bus_service_time = case_when(bus_route_door_cond == "Series" ~ bus_service_time_alight + bus_service_time_board, 
                                          T~max(bus_service_time_alight, bus_service_time_board))) %>% 
      mutate(bus_service_stop = bus_board_start + bus_service_time) %>% 
      mutate(bus_delay_exit = 0, 
             # bus_delay_exit = case_when(bus_service_stop < bus_exit_actual_init ~ (bus_exit_actual_init-bus_service_stop),
             #                            T~make_bus_delay(xtra_delay_list, bus_delay_entry, bus_service_time)),
             bus_time_inBerth = bus_delay_exit + bus_service_time,
             bus_exit_actual = bus_board_start + bus_time_inBerth,
             bus_delayed_by_queue = case_when(bus_delay_entry == 0~0,
                                              bus_delay_entry != 0~1),
             bus_pass_picked_up = bus_surplus_seats-sum(is.na(pass_id)),
             bus_delay_total = bus_delay_entry + bus_delay_exit,
             bus_total_operation = bus_delay_entry  + bus_service_time + bus_delay_exit,
             bus_no_pick_up = case_when(bus_pass_picked_up == 0~0,
                                        bus_pass_picked_up != 0~1)
      ) 
    
    print(bus_exit_actual_init)
    bus_exit_actual_init = left_list[[i]][[1, "bus_exit_actual"]][[1]]
    bus_interference_check = (bus_exit_actual_init < df_bus[i+1, "bus_arrvl_actl"][[1]]) #FALSE means interference
    bus_interference_check = ifelse(is.na(bus_interference_check), T, bus_interference_check)
    
    if (bus_interference_check){
      #no interference always reset berth assignment to 1
      current_open_bay = 1
      current_queue_penalty = 0
    } else {
      if (current_open_bay == berths) {
        #if at last berth, assign to first with time penalty
        current_open_bay = 1
        current_queue_penalty = (bus_exit_actual_init - df_bus[i+1, "bus_arrvl_actl"][[1]])
      } else {
        current_open_bay = current_open_bay + 1
        current_queue_penalty = 0
      }
    }
    
    already_left = c(already_left, 
                     left_list[[i]] %>%
                       na.omit() %>% 
                       .[["pass_id_line"]])
  }
  
  #reduces all list elements to single dataframe with passengers sorted to buses 
  df_left = left_list  %>%  
    reduce(bind_rows) %>% 
    mutate(pass_wait = bus_board_start-pass_arrvl)  
  
  #returns passengers who are left
  df_remain = df_pass %>%  
    filter(pass_id %not_in% df_left$pass_id)
  
  return((list(df_bus, df_pass, df_left, df_remain)))
}

#get metrics from simulation object=============================================
get_summary_df = function(simulation_results_object, length, df){
  list(simulation_results_object, 1:as.numeric(length), df) %>%
    pmap(function(x, y, z)
      x[[z]] %>%
        mutate(simulation_num = y)
    ) %>%
    reduce(bind_rows)
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

  