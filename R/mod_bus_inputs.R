#' bus_inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bus_inputs_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("bus_inputs"))

  )
}

#' bus_inputs Server Function
#'
#' @noRd
mod_bus_inputs_server <- function(input, output, session, route_num){
  ns <- session$ns

  output$bus_inputs <- renderUI({
    box(
      textInput(inputId = paste0("bus_route_num_", route_num), label = "Route Name", placeholder = "Unique route name here"),
      strong("Bus arrival metrics:"),
      hr(),
      splitLayout(cellWidths = c("40%", "40%"),
                  sliderInput(inputId  = paste0("bus_route_headway_", route_num) , label = "Headway (min)", min = 5, max = 60, step = 5, value = 15),
                  sliderInput(inputId  = paste0("bus_route_headway_sd", route_num) , label = "Std. Deviation (min)", min = 1, max = 10, step = 1, value = 3)
      ),
      strong("Bus metrics:"),
      hr(),
      splitLayout(cellWidths = c("30%", "50%"),
                  selectInput(inputId  = paste0("bus_route_size_", route_num), label = "Select Bus Size", choices = c(30, 60), selected = 60),
                  sliderInput(inputId  = paste0("bus_route_cap_", route_num), label = "Capacity (%)", min = 10, max = 90, step = 10, value = 60)
      )
    )
  })
}

## To be copied in the UI
# mod_bus_inputs_ui("bus_inputs_ui_1")

## To be copied in the server
# callModule(mod_bus_inputs_server, "bus_inputs_ui_1")

# shiny_input_bus_1_route = "506" #in minutes
# shiny_input_bus_1_headway = 15 #in minutes
# shiny_input_bus_1_headway_sd = 3 #in minutes
# shiny_input_bus_1_capacity = 50 #in persons
# shiny_input_bus_1_spare = .63 #in percent


