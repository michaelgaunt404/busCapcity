#' get_variables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_get_variables_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton("view_var", "View Inputs")
 
  )
}
    
#' get_variables Server Functions
#'
#' @noRd 
mod_get_variables_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
      rv_df_bus <<- df_bus()
      rv_df_pass <<- df_pass()
      rv_exit <<- exit_condition_inputs()
      rv_RVlist <<- RVlist()
      rv_pass_inputs <<- pass_inputs()
      sim <<- simulation_results()
    })
 
  })
}
    
## To be copied in the UI
# mod_get_variables_ui("get_variables_ui_1")
    
## To be copied in the server
# mod_get_variables_server("get_variables_ui_1")
