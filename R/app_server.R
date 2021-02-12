#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  simul_num_routes = reactive({
    input$simul_num_routes %>% 
      as.numeric()
    })
  
  output$bus <- renderUI({
    lapply(1:simul_num_routes(), function(i){
      callModule(mod_bus_inputs_server, "bus_inputs_ui_1",
                 route_num = i)
      })
  })
  
  RVlist = reactive({
    reactiveValuesToList(input)
  })
  
  pass_inputs = eventReactive(input$bus_input_go, {
    # list(
    #   input$simul_duration,
    #   input$pass_board,
    #   input$pass_board_sd
    # )
    
    get_list_items(RVlist(), string = "simul_duration|pass_board", purrr = F)
  }) 
  
  exit_condition_inputs = eventReactive(input$bus_input_go, {
    get_list_items(RVlist(), string = "exit_cond_", purrr = F)
  }) 
  
  df_bus = eventReactive(input$bus_input_go, {
    get_bus_inputs(RVlist(), input$simul_num_routes, pass_inputs())
  })
  
  df_pass = eventReactive(input$bus_input_go, {
    get_pass_inputs(RVlist(), input$simul_num_routes, pass_inputs())
  })
  
  observe({
    rv_df_bus <<- df_bus()
    rv_df_pass <<- df_pass()
    rv_exit <<- exit_condition_inputs()
    rv_RVlist <<- RVlist()
    rv_pass_inputs <<- pass_inputs()
    sim <<- simulation_results()
  })
  
  output$smmry_bus_routes = DT::renderDataTable({
    df_bus() %>%
      select(bus_line, bus_id, bus_route_cap, bus_arrvl_schl, bus_arrvl_actl) %>%
      dt_common(dom = "Bftir",
                y = 500, pl = 800)
  })
  
  simulation_results = eventReactive(input$bus_simulation_go, {
    busCapacityCalculate(df_bus(), df_pass(), exit_condition_inputs(), as.numeric(input$simul_num_berths))
  })
  
  output$results_summary_stats = DT::renderDataTable({
    bind_rows(get_summry_statistics(simulation_results()[[1]][[3]]),
              get_summry_statistics(simulation_results()[[1]][[3]], grouped = T, group = "bus_line")) %>%
      mutate(bus_line = case_when(is.na(bus_line) ~ "All Buses",
                                  T ~ bus_line)) %>%
      select(bus_line, everything()) %>%
      dt_common(dom = "Bftir",
                y = 500, pl = 8000)
  })


  output$results_bus_assign = DT::renderDataTable({
    simulation_results()[[1]][[3]] %>%
      select(bus_line, bus_line_id, starts_with("bus")) %>%
      unique() %>%
      dt_common(dom = "Bftir",
                y = 500, pl = 8000)
  })

  output$results_pass_not_picked_up = DT::renderDataTable({
    simulation_results()[[1]][[4]] %>%
      dt_common(dom = "Bftir",
                y = 500, pl = 8000)
  })

  output$pass_arrvl_dist = renderPlotly({
    chart = df_pass() %>%
      ggplot() +
      geom_freqpoly(aes(pass_arrvl, color = bus_line), binwidth = 60*5) +
      labs(x = "Passenger Arrival (sec)", title = "Distribution of Passenger Arrivals") +
      scale_color_viridis_d() +
      theme_classic()

    plotly::ggplotly(chart)

  })
  
}




