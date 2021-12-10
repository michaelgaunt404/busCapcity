#' glassary_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_glassary_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
    col_4(
      box(title = "Simulation Module Description" 
          ,closable = F ,collapsed = F ,collapsible = T 
          ,width = "100%" ,solidHeader = T, status = "primary"
          ,DT::dataTableOutput(ns("module_list"))
      )
    ),
    col_8(
      box(title = "Simulation Variable Dictionary" 
          ,closable = F ,collapsed = F ,collapsible = T 
          ,width = "100%" ,solidHeader = T, status = "primary"
          ,DT::dataTableOutput(ns("variable_list"))
      ) 
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
    
#' glassary_tab Server Functions
#'
#' @noRd 
mod_glassary_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
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
 
  })
}
    
## To be copied in the UI
# mod_glassary_tab_ui("glassary_tab_ui_1")
    
## To be copied in the server
# mod_glassary_tab_server("glassary_tab_ui_1")
