#' output_dt UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_output_dt_ui <- function(id){
  ns <- NS(id)
  tagList(
    # downloadBttn(
    #   label = "Download Raw Data", size = "sm",
    #   outputId = "downloadData", style = "bordered",
    #   color = "primary"
    # ),
    tabBox(
      width = "100%",
      title = "Simulation Summary Results",
      id = "tabset1", 
      height = "250px",
      tabPanel(
        "Statistics for all Variables", 
        DT::dataTableOutput(ns("bus_summary_table_skimmed"))
      ),
      tabPanel(
        "Bus Metrics", 
        DT::dataTableOutput(ns("bus_summary_table")) %>%  
          withSpinner()
      ),
      tabPanel(
        "Passenger Metrics", 
        DT::dataTableOutput(ns("pass_summary_table"))
      )
    )
  )
}
    
#' output_dt Server Function
#'
#' @noRd 
mod_output_dt_server <- function(input, output, session, .data){
  ns <- session$ns
  
  output$bus_summary_table_skimmed = DT::renderDataTable({
    .data()[[3]] %>% 
      skimr::skim() %>%  
      skimr::yank("numeric") %>% 
      rename("variable" = skim_variable) %>% 
      mutate(across(c(complete_rate:sd), dgt2)) %>% 
      dt_common(dom = "Bftir",
                y = 600, pl = 8000)
  })
  
  # addPopover(session, "bus_summary_table_skimmed", "Data", content = "Hi")
  
  output$bus_summary_table = DT::renderDataTable({
    .data()[[3]] %>% 
      select(starts_with('bus_'), "simulation_num") %>%  
      unique() %>%
      dt_common(dom = "Bftir",
                y = 600, pl = 8000)
  })
  
  output$pass_summary_table = DT::renderDataTable({
    bind_rows(
      .data()[[3]], 
      .data()[[4]]
    ) %>% 
      select(-starts_with('bus_')) %>%  
      unique() %>%
      dt_common(dom = "Bftir",
                y = 600, pl = 8000)
  })
  
}
    
## To be copied in the UI
# mod_output_dt_ui("output_dt_ui_1")
    
## To be copied in the server
# callModule(mod_output_dt_server, "output_dt_ui_1")
 
