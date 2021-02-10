

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

# df_bus = rv_df_bus
rvList = rv_RVlist
# df_pass = rv_df_pass
# xtra_delay_list = rv_exit
# df_input = rv_pass_inputs
num_of_buses = 3
pass_input_list = rv_pass_inputs
# simul_time = rv_pass_inputs[[1]]

df_bus = get_bus_inputs(rvList, num_of_buses, pass_input_list[[1]])
df_pass = get_pass_inputs(rvList, num_of_buses, pass_input_list)

data = busCapacityCalculate(df_bus, df_pass, xtra_delay_list)[[1]][[3]]



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








