#' output_dt UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 

mod_glossary_ui <- function(id){
  ns <- NS(id)
  tagList(
    col_3(DT::dataTableOutput(ns("module_list"))
          ),
    col_9(
      DT::dataTableOutput(ns("variable_list"))
    )

    # tabBox(
    #   width = "100%",
    #   title = "Simulation Summary Results",
    #   id = "tabset1", 
    #   height = "250px",
    #   tabPanel(
    #     "Quick Statistics", 
    #     DT::dataTableOutput(ns("bus_summary_table_skimmed"))
    #   ),
    #   tabPanel(
    #     "Bus Summary Table", 
    #     DT::dataTableOutput(ns("bus_summary_table")) %>%  
    #       withSpinner()
    #   ),
    #   tabPanel(
    #     "Pass Summary Table", 
    #     DT::dataTableOutput(ns("pass_summary_table"))
    #   )
    # )
  )
}
    
#' output_dt Server Function
#'
#' @noRd 
mod_glossary_server <- function(input, output, session, .data){
  ns <- session$ns
  
  output$module_list = DT::renderDataTable({
    simulation_modules %>%
      dt_common(dom = "Bftir",
                y = 600, pl = 8000)
  })
  
  output$variable_list = DT::renderDataTable({
    variable_list_glos %>%
      dt_common(dom = "Bftir",
                y = 600, pl = 8000)
  })

}
    
## To be copied in the UI
# mod_glossary_ui("glossary")
    
## To be copied in the server
# callModule(mod_glossary_server, "glossary")
 
