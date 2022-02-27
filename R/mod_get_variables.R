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
    actionButton(ns("view_var"), "View Inputs")
 
  )
}
    
#' get_variables Server Functions
#'
#' @noRd 
mod_get_variables_server <- function(id, .data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$view_var, {
      #dont need these because I already do that in the server file
      #probably should be put into here but screw it
      
      # observe({
      #   rv_df_bus <<- df_bus()
      #   rv_df_pass <<- df_pass()
      #   rv_exit <<- exit_condition_inputs()
      #   rv_RVlist <<- RVlist()
      #   rv_pass_inputs <<- pass_inputs()
      #   sim <<- simulation_results()
      # })
      
      global_rv <<- .data()

      .data() %>%  
        as.matrix() %>% 
        data.frame(variables = row.names(.), .) %>% 
        `rownames<-`(seq_len(nrow(.))) %>%  
        arrange(variables) %>%  
        filter(!str_detect(variables, "glassary")) %>% 
        print()
      
    })
    
  })
}
    
## To be copied in the UI
# mod_get_variables_ui("get_variables_ui_1")
    
## To be copied in the server
# mod_get_variables_server("get_variables_ui_1")
