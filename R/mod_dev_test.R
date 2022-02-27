#' dev_test UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_dev_test_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' dev_test Server Functions
#'
#' @noRd 
mod_dev_test_server <- function(id, item){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(item, {
    print("helloisjudsijdhishd")
    })
 
  })
}
    
## To be copied in the UI
# mod_dev_test_ui("dev_test_ui_1")
    
## To be copied in the server
# mod_dev_test_server("dev_test_ui_1")
