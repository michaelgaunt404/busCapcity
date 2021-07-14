#' bs_modals UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_bs_modals_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("bs_modals"))
      
  )
}
    
#' bs_modals Server Function
#'
#' @noRd 
mod_bs_modals_server <- function(input, output, session, id){
  ns <- session$ns
  
  output$bs_modals <- renderUI({
    tagList(
      bsButton(inputId = id, label = "Info", icon = icon("question"), style = "info", size = "extra-small"),
      bsPopover(id = id, title = "title",
                content = "yo",
                placement = "right", 
                trigger = "click")
    )
  })
  
  
  
 
}
    
## To be copied in the UI
# mod_bs_modals_ui("bs_modals_ui_1")
    
## To be copied in the server
# callModule(mod_bs_modals_server, "bs_modals_ui_1", id = "hel")
 
