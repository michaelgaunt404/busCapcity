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
  
  #makes reacrtive number of bus inputs=========================================
  output$bus <- renderUI({
    lapply(1:simul_num_routes(), function(i){
      callModule(mod_bus_inputs_server, 
                 "bus_inputs_ui_1",
                 route_num = i)
      })
  })
  
  #SECTION: popup messages/modals===============================================
  #=============================================================================
  observeEvent(input$contact, {
    sendSweetAlert(session = session, title = NULL, html = TRUE, btn_labels = c('Close'), text =
                     tags$span(style = 'text-align: left;',
                               tags$h3('Contact Us', style = 'color: #d73926;'),
                               tags$h4('The Data Informatics Group', style = 'font-weight: 700;'),
                               tags$p('Based in Seattle, the Data Informatics Group specializes in creating bespoke data \
                              products that daylight powerful insights and enable our clients to harness the \
                              full-potential of their data. Reach out to us!'),
                               tags$div(id = 'contact_table', render_contact_table()))
    )
  })
  
  observeEvent(input$dist_board, {
    sendSweetAlert(session = session, title = NULL, html = TRUE, btn_labels = c('Close'), text =
                     tags$span(style = 'text-align: left;',
                               tags$div(id = 'contact_table',renderPlot(make_histogram(input$pass_board, input$pass_board_sd, lmt = T))
                                        ))
    )
  })
  
  observeEvent(input$dist_alight, {
    sendSweetAlert(session = session, title = NULL, html = TRUE, btn_labels = c('Close'), text =
                     tags$span(style = 'text-align: left;',
                               tags$div(id = 'contact_table', renderPlot(make_histogram(input$pass_alight, input$pass_alight_sd, lmt = T))
                                        ))
    )
  })
  
  observeEvent(input$view_var, {
  print(RVlist())
  #   sendSweetAlert(session = session, title = NULL, html = TRUE, btn_labels = c('Close'), text =
  #                    tags$span(style = 'text-align: left;',
  #                              tags$div(id = 'contact_table', 
  #                                       DT::renderDataTable({
  #                                         RVlist() %>%
  #                                           dt_common(dom = "Bftir",
  #                                                     y = 600, pl = 8000)
  #                                       })  
  #                              ))
  #   )
  })
  

  RVlist = reactive({
    reactiveValuesToList(input)
  })
  
  pass_inputs = eventReactive(input$bus_input_go, {
    print("pass_inputs")
    get_list_items(RVlist(), string = "simul_duration|pass_board", purrr = F)
  }) 
  
  exit_condition_inputs = eventReactive(input$bus_input_go, {
    print("exit_condition_inputs")
    get_list_items(RVlist(), string = "exit_cond_", purrr = F)
  }) 
  
  df_bus = eventReactive(input$bus_input_go, {
    require(pass_inputs())
    print("df_bus")
    get_bus_inputs(RVlist(), input$simul_num_routes, pass_inputs())
    # get_bus_inputs(rv_RVlist, rv_RVlist$simul_num_routes, rv_pass_inputs)
    print("df_bus_end")
  })
  
  df_pass = eventReactive(input$bus_input_go, {
    print("df_pass")
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
                y = 290, pl = 800)
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
  
  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     paste('data-', Sys.Date(), '.csv', sep='')
  #   },
  #   content = function(con) {
  #     write.csv(data, con)
  #   }
  # )
  
  simulation_results = eventReactive(input$bus_simulation_go, {
    tmp_raw = list(
      map(1:as.numeric(input$simul_num), function(m) get_bus_inputs(RVlist(), input$simul_num_routes, pass_inputs())),
      map(1:as.numeric(input$simul_num), function(m) get_pass_inputs(RVlist(), input$simul_num_routes, pass_inputs()))  
    ) %>%  
      pmap(function(x, y, z)
        busCapacityCalculate(x, y, rv_exit, 1)
      )
    map(1:4, function(x) get_summary_df(tmp_raw, as.numeric(input$simul_num), x))
    
  })
 
  output$results_summary_stats = DT::renderDataTable({
    bind_rows(get_summry_statistics(simulation_results()[[3]]),
              get_summry_statistics(simulation_results()[[3]], grouped = T, group = "bus_line")) %>%
      mutate(bus_line = case_when(is.na(bus_line) ~ "All Buses",
                                  T ~ bus_line)) %>%
      select(bus_line, everything()) %>%
      dt_common(dom = "Bftir",
                y = 600, pl = 8000)
  })

  ## To be copied in the server
  callModule(mod_output_dt_server, "summary_tab", .data = simulation_results)


  observe({
    require(input$simul_num_routes)
    
    # Sys.sleep(2)
    
    #bus route capacity
    list(1:as.numeric(input$simul_num_routes), "bus_route_cap_", "Must be 0 <> 100", ">100") %>%
      pmap(function(x, y, z, m)
        observeEvent(input[[paste0(y, x)]], {
          if (input[[paste0(y, x)]] > 100) {
            showFeedbackWarning(
              inputId =  paste0(y, x),
              text = z
            )
          } else {
            hideFeedback(paste0(y, x))
          }
        }
        )
      )
    
    #bus route passengers
    list(1:as.numeric(input$simul_num_routes), "bus_route_pass_", "Change 0 to 1", "==0") %>%
      pmap(function(x, y, z, m)
        observeEvent(input[[paste0(y, x)]], {
          if (input[[paste0(y, x)]] < 1) {
            showFeedbackWarning(
              inputId =  paste0(y, x),
              text = z
            )
          } else {
            hideFeedback(paste0(y, x))
          }
        }
        )
      )
    
    #plot modals for bus inputs=================================================
    #===========================================================================
    #modals for capacity count
    list(1:as.numeric(input$simul_num_routes), "bus_size_", "bus_route_size_", "bus_route_cap_") %>%
      pmap(function(x, y, z, m)
        observeEvent(input[[paste0(y, x)]], {
          sendSweetAlert(session = session, title = NULL, html = TRUE, btn_labels = c('Close'), text =
                           tags$span(style = 'text-align: left;',
                                     tags$div(id = 'contact_table', DT::renderDT(
                                       data.frame(Variable = c("Bus Maximum Capcity", "No. of Empty Seats", "No. Passengers Onboard"), 
                                                  Count = c(input[[paste0(z, x)]], 
                                                            input[[paste0(z, x)]]*(input[[paste0(m, x)]])/100, 
                                                            input[[paste0(z, x)]]-input[[paste0(z, x)]]*(input[[paste0(m, x)]])/100)) %>%  
                                         dt_common(dom = "t")
                                       
                                       )
                                     ))
          )}
        )
      )
    
    #modals for headway density
    list(1:as.numeric(input$simul_num_routes), "dist_headway_", "bus_route_headway_", "bus_route_headway_sd_") %>%
      pmap(function(x, y, z, m)
        observeEvent(input[[paste0(y, x)]], {
          sendSweetAlert(session = session, title = NULL, html = TRUE, btn_labels = c('Close'), text =
                           tags$span(style = 'text-align: left;',
                                     tags$div(id = 'contact_table', renderPlot(make_density(input[[paste0(z, x)]], input[[paste0(m, x)]]))
                                     ))
          )}
        )
      )

    #modals for alight density
    list(1:as.numeric(input$simul_num_routes), "dist_route_num_alight_", "bus_route_num_alight_", "bus_route_num_alight_sd_") %>%
      pmap(function(x, y, z, m)
        observeEvent(input[[paste0(y, x)]], {
          sendSweetAlert(session = session, title = NULL, html = TRUE, btn_labels = c('Close'), text =
                           tags$span(style = 'text-align: left;',
                                     tags$div(id = 'contact_table', renderPlot(make_density(input[[paste0(z, x)]], input[[paste0(m, x)]]))
                                     ))
          )}
        )
      )

    #modals for passenger density
    list(1:as.numeric(input$simul_num_routes), "dist_route_pass_", "bus_route_pass_", "bus_route_pass_sd_") %>%
      pmap(function(x, y, z, m)
        observeEvent(input[[paste0(y, x)]], {
          sendSweetAlert(session = session, title = NULL, html = TRUE, btn_labels = c('Close'), text =
                           tags$span(style = 'text-align: left;',
                                     tags$div(id = 'contact_table', renderPlot(make_density(input[[paste0(z, x)]], input[[paste0(m, x)]]))
                                     ))
          )}
        )
      )

  })
  
}




