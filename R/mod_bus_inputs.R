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
      width = 6,
      style = "background:#F5F5F5",
      
      splitLayout(cellWidths = c("30%", "30%", "30%"),
                  textInput(inputId = paste0("bus_line_", route_num), label = "Route Name:", 
                            value = paste0("place_holder_", route_num)
                            # ,
                            # placeholder = "Unique route name here" #NOTE- want to change this to have palce holder and use the shinyfeedback pkg
                  ),
                  sliderInput(inputId  = paste0("bus_route_pass_", route_num) , label = "Pass./hr", min = 50, max = 2000, step = 50, value = 200), 
                  sliderInput(inputId  = paste0("bus_route_pass_sd_", route_num) , label = "Pass Arrvl. std (sec):", min = 0, max = 90, step = 5, value = 15)
      ),
      # strong("Bus arrival metrics:"),
      # hr(),
      splitLayout(cellWidths = c("30%", "30%"),
                  sliderInput(inputId  = paste0("bus_route_headway_", route_num) , label = "Headway (min)", min = 5, max = 60, step = 5, value = 15),
                  sliderInput(inputId  = paste0("bus_route_headway_sd_", route_num) , label = "Std. Deviation (min)", min = 1, max = 10, step = 1, value = 3)
      ),
      # strong("Bus metrics:"),
      # hr(),
      splitLayout(cellWidths = c("20%", "30%"),
                  selectInput(inputId  = paste0("bus_route_size_", route_num), label = "Select Bus Size", choices = c(30, 60), selected = 60),
                  sliderInput(inputId  = paste0("bus_route_cap_", route_num), label = "Incoming Capacity (%)", min = 10, max = 90, step = 10, value = 60)
      )
      # , 
      # sliderInput(inputId  = paste0("bus_route_headway_", route_num) , label = "Headway (min)", min = 5, max = 60, step = 5, value = 15)
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


