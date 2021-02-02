#' simulation_inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_simulation_inputs_ui <- function(id){
  ns <- NS(id)
  tagList(
    # boxPlus_common("Simulation Inputs", 
    boxPlus(title = "Simulation Inputs", 
            closable = F, 
            collapsed = F, 
            collapsible = T, 
            width = "100%", 
            solidHeader = T, 
            status = "primary", 
            
            sliderInput("simul_duration",
                        "Simulation Duration (min):", 
                        min = 30, 
                        max = 240, 
                        step = 15,
                        value = 60), 
            # fluidRow(
              splitLayout(cellWidths = c("80%", "20%"), 
                          selectInput("exit_cond", "Select bus berth exit condition:", 
                                      choices = c("Free Exit" = 1, 
                                                  "Obstructed" = 2, 
                                                  "Signal" = 3, 
                                                  "Merge" = 4),
                                      selected = "Signal"),
                          actionButton("info_exit_cond", "Info", icon = icon("question-circle"), style = "margin-top: 25px")
                          # bsButton("q1", label = "click me", icon = icon("question"), style = "margin-top: 30px", size = "extra-small")
                          #i'd like tpo implement this bs option at some point
              # )
            ),
            splitLayout(cellWidths = c("40%", "40%", "20%"), 
                        selectInput("simul_num_routes",
                                    "Routes to simulate:", 
                                    choices = c(1:4),
                                    selected = 1), 
                        selectInput("simul_num_berths",
                                    "Berths at stop:", 
                                    choices = c(1:1),
                                    selected = 1)
            ),
            strong("Passeger board/alight times (sec):"),
            splitLayout(cellWidths = c("40%", "40%", "20%"), 
                        sliderInput("pass_board",
                                    "Avg Duration:", 
                                    min = 1, 
                                    max = 10, 
                                    step = .1,
                                    value = 2.1), 
                        sliderInput("pass_board_sd",
                                    "Std. Deviation:", 
                                    min = 0, 
                                    max = 10, 
                                    step = 1,
                                    value = 0),
                        actionButton("info_glbl_pass", "Info", icon = icon("question-circle"), style = "margin-top: 25px")
            )
    )
  )
}
    
#' simulation_inputs Server Function
#'
#' @noRd 
mod_simulation_inputs_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_simulation_inputs_ui("simulation_inputs_ui_1")
    
## To be copied in the server
# callModule(mod_simulation_inputs_server, "simulation_inputs_ui_1")
 
