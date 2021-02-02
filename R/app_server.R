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
  
  observe({
    print(input$simul_duration)
    print(simul_num_routes())
  })
  
  # observe({
  #   req(simul_num_routes())
  #   callModule(mod_bus_inputs_server, "bus_inputs_ui_1",
  #              route_num = simul_num_routes())
  # })
  
  output$bus <- renderUI({
    lapply(1:simul_num_routes(), function(i){
      callModule(mod_bus_inputs_server, "bus_inputs_ui_1",
                 route_num = i)
      })
  })
  
  observe({
    # map(1:simul_num_routes(), 
    #     function(i) paste0("bus_route_headway_", i) %>% 
    #       input[[.]]
    # )
    
    # print(AllInputs())
    # print(reactiveValuesToList(input))
    
    RVlist <<- reactiveValuesToList(input)
  })
  
  
  # AllInputs <- reactive({
  #   x <- reactiveValuesToList(input)
  #   data.frame(
  #     names = names(x),
  #     values = unlist(x, use.names = FALSE)
  #   )
  # })
  
  
}
