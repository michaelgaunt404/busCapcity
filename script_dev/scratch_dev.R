

make_bus_delay(xtra_delay_list, bus_delay_entry, bus_service_time)

###note::::::make extra delay list

observe({
  print(pass_inputs())
  rv_df_bus <<- df_bus()
  rv_RVlist <<- RVlist()
  rv_pass_inputs <<- pass_inputs()
})



rv_df_bus #<<- df_bus()
rv_df_pass #<<- df_pass()
rv_exit #<<- exit_condition_inputs()
rv_RVlist #<<- RVlist()
rv_pass_inputs #<<- pass_inputs()
rvList = rv_RVlist
num_of_buses = 1 
pass_input_list = rv_pass_inputs
sim #<<- simulation_results()
input = list(simul_num_routes = 1, 
             simul_num = 5)

df_bus = rv_df_bus
df_pass = rv_df_pass
xtra_delay_list = rv_exit
berths = 1

rv_RVlist$exi

rvList = rv_RVlist
num_of_buses = 3

input_list = rv_pass_inputs
input = list(simul_num_berths = 1)
input$simul_num_berths

df_bus = get_bus_inputs(rv_RVlist, 1, rv_pass_inputs)
df_pass = get_pass_inputs(rv_RVlist, 1, rv_pass_inputs)
rvList, num_of_buses, input_list

tmp_raw = list(
  map(1:as.numeric(input$simul_num), function(m) get_bus_inputs(rv_RVlist, input$simul_num_routes, rv_pass_inputs)),
  map(1:as.numeric(input$simul_num), function(m) get_pass_inputs(rv_RVlist, input$simul_num_routes, rv_pass_inputs))  
) %>%  
  pmap(function(x, y, z)
    busCapacityCalculate(x, y, rv_exit, 1)
  )

map(1:4, function(x) get_summary_df(tmp_raw, as.numeric(input$simul_num), x))






# list(sim, c("Bus Input", "Pass Input", "Simulation Output", "Missed Passengers List")) %>%  
#   pmap(~ data.frame(data = .y,  
#                    names_raw = colnames(.x))) %>%  
#   reduce(bind_rows) %>%  
#   mutate(names_p1 = names_raw %>%  
#            str_replace_all("_", " ") %>%  
#            str_to_title()) %>%  
#   write.csv("./data_dev/names_correction.csv")

       