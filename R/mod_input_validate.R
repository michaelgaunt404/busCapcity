#' input_validate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_input_validate_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("bus_input_go_2"), "bus_input_go_2")
  )
}
    
#' input_validate Server Functions
#'
#' @noRd 
mod_input_validate_server <- function(id, .data, .test){
  #to make a module that can take an external input
  #---->like .data needs a .something placeholder
  #---->input needs to be wrapped in reactive wrapper
  
  #to make a module a reactive value 
  #---->need to make var with eventReactive
  #---->need to return the value made from eventReactive

  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    var_check_bus_rv = eventReactive(.test(), {
      
      index_bus_inputs = .data() %>%  
        names() %>%  
        str_detect("bus_route_")
      
      
      check = .data()[index_bus_inputs] %>%
        some(~is.na(.x))
      
      if (check) {
        show_alert(title = "Invalid Input(s)!!!", 
                   text = "One or more of your bus inputs are invalid. Please reivew your inputs and change any invalid or missing inputs.",
                   type = "error")
        var_check_bus <<- FALSE
      } else {
        var_check_bus <<- TRUE
      }
      return(var_check_bus)
    })
    
    return(var_check_bus_rv)
  })
}

## To be copied in the UI
# mod_input_validate_ui("input_validate_ui_1")
    
## To be copied in the server
# mod_input_validate_server("input_validate_ui_1")
