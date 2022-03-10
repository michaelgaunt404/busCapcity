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
  
  #SECTION: data initialization=================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  modal_counter <<- 0
  
  var_check_bus <<-F

  RVlist = reactive({
    reactiveValuesToList(input)
  })
  
  pass_inputs = eventReactive(input$bus_input_go, {
    req(val_bus_input() == T)
    print("pass_inputs")
    get_list_items(RVlist(), string = "simul_duration|pass_board", purrr = F)
  }) 
  
  exit_condition_inputs = eventReactive(input$bus_input_go, {
    req(val_bus_input() == T)
    print("exit_condition_inputs")
    get_list_items(RVlist(), string = "exit_cond_", purrr = F)
  }) 
  
  df_bus = eventReactive(input$bus_input_go, {
    req(val_bus_input() == T)
    # require(pass_inputs())
    print("df_bus")
    get_bus_inputs(RVlist(), input$simul_num_routes, pass_inputs())
    # get_bus_inputs(rv_RVlist, rv_RVlist$simul_num_routes, rv_pass_inputs)
    # print("df_bus_end")
  })
  
  df_pass = eventReactive(input$bus_input_go, {
    req(val_bus_input() == T)
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
  
  #SECTION: call modules========================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  callModule(mod_output_dt_server, "summary_tab", .data = simulation_results)
  
  mod_result_viz_server("yolo_check", .data = simulation_results)
  
  mod_glassary_tab_server("glassary_tab_ui_1")
  
  mod_get_variables_server("get_variables_ui_1", .data = RVlist)
  
  val_bus_input = mod_input_validate_server("input_validate_ui_1", 
                                    .data = RVlist, 
                                    .test = reactive(input$bus_input_go))
  
  
  #SECTION: data initialization=================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$smmry_bus_routes = DT::renderDataTable({
    df_bus() %>%
      select(bus_line, bus_id, bus_route_cap, bus_arrvl_schl, bus_arrvl_actl) %>%
      dt_common(dom = "Bftir",
                y = 290, pl = 800)
  })
  
  output$pass_arrvl_cumm = renderPlotly({
    temp = df_pass()  %>%  
      # rv_df_pass %>%
      group_by(bus_line) %>% 
      mutate(total = dplyr::row_number()) 
    
    temp %>%  
      plotly::plot_ly(x=~pass_arrvl/60,y=~total, color=~bus_line, mode = "lines") %>% 
      plotly::layout(xaxis = list(title = "Arrival Time (minutes)"), 
                     yaxis = list(title = "Count"),
                     showlegend = T,legend = list(orientation = 'h',x = 0, y = 1))
  })
  
  output$pass_arrvl_hist = renderPlotly({
    temp = 
      df_pass() %>%  
      # rv_df_pass %>%
      mutate(pass_arrvl = (pass_arrvl/60)) %>% 
      ggplot() + 
      geom_histogram(aes(pass_arrvl, fill = bus_line), binwidth = 5) + 
      facet_grid(rows = vars(bus_line)) + 
      theme_classic() + 
      labs(x = "Arrival Time (minutes)", 
           y = "Count")
    
    plotly::ggplotly(temp) %>%  
      plotly::layout(showlegend = T,legend = list(orientation = 'h',x = 0, y = 1.05))
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
    tmp_raw <<- list(
      map(1:as.numeric(input$simul_num), function(m) get_bus_inputs(RVlist(), input$simul_num_routes, pass_inputs())),
      map(1:as.numeric(input$simul_num), function(m) get_pass_inputs(RVlist(), input$simul_num_routes, pass_inputs()))  
    ) 
    
    print(input$simul_num_berths)
    
    temp_raw = tmp_raw %>%  
      pmap(function(x, y, z)
        busCapacityCalculate(x, y, rv_exit, input$simul_num_berths)
      )
    
    map(1:4, function(x) get_summary_df(temp_raw, as.numeric(input$simul_num), x))
    
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #SECTION: popup messages/modals===============================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #general modals that do not need to be wrapped by any special event wrapper
  
  observeEvent(req(modal_counter == 0), {
    showModal(modalDialog(
      includeHTML("./inst/app/www/modal_intro.html"),
      size = "l",
      easyClose = TRUE
    ))
    modal_counter %+=% 999
  })
  
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
  
  
  
  ###SUBSECTION: popup messages/modals==========================================
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #special modals which are programmaticly made using bus inputs
  #need to wrapped in observe statement
  
  observe({
    req(input$simul_num_routes)

    
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

    
    observeEvent(req(input$dist_board), {
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
    
    #plot modals for bus inputs=================================================
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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




