#' sim_inputs_noBox UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sim_inputs_noBox_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton("bus_input_go", "View Bus Inputs") %>%  
      tipify(title = "Step 1: Click this button to review the inputs you have provided below."),
    actionButton("bus_simulation_go", "Run Simulation") %>%  
      tipify(title = "Step 2: If you have reviewed your inputs, click this button to intiate the simulations."), 
    hr(),
    splitLayout(cellWidths = c("50%", "40%"),
                numericInput("simul_duration",
                             "Simulation Duration (min):", 
                             min = 30, 
                             max = 60*6, 
                             step = 1,
                             value = 60), 
                numericInput("simul_num",
                             "Number of Simulations", 
                             min = 1, 
                             max = 100, 
                             step = 1,
                             value = 1) %>%
                  tipify("Please select the number of simulations you'd like to run, it is suggested to run only 1 simulation if you are unfamiliar with this application.")
    ),
    splitLayout(cellWidths = c("50%", "40%"),
                # style="display:inline-block;vertical-align:bottom;",
                numericInput("simul_num_routes",
                             "Routes to simulate:", 
                             min = 1, 
                             max = 4, 
                             step = 1,
                             value = 1) %>%
                  tipify("Please select the number of simulations you'd like to run, it is suggested to run only 1 simulation if you are unfamiliar with this application."), 
                numericInput("simul_num_berths",
                             "Berths at stop:", 
                             min = 1, 
                             max = 3, 
                             step = 1,
                             value = 1) %>%
                  tipify("Please select the number of simulations you'd like to run, it is suggested to run only 1 simulation if you are unfamiliar with this application.")
    ),
    strong("Passeger Boarding Time (sec/passenger):"),
    splitLayout(cellWidths = c("40%", "40%", "20%"), 
                numericInput("pass_board",
                             "Duration Mean:", 
                             min = 1, 
                             max = 10, 
                             step = .1,
                             value = 2.1) %>% 
                  tipify(title = "This input defines how lond it takes for a passenger to board and alight behavior for all buses and routes."), 
                numericInput("pass_board_sd",
                             "Duration SD:", 
                             min = 0, 
                             max = 10, 
                             step = 1,
                             value = 0) %>% 
                  tipify(title = "Expected standard deviation of metric (left). Standard deviation uses a normal distribution centered around zero (no delay, or early or late arrival). A zero standard deviation result in no deviation from the mean."), 
                actionButton("dist_board", label = icon("eye")),
                tags$style(type='text/css', "#dist_board {  margin-top: 25px;}")
    ),
    strong("Passeger Alighting Time (sec/passenger):"),
    splitLayout(cellWidths = c("40%", "40%", "20%"), 
                numericInput("pass_alight",
                             "Duration Mean:", 
                             min = 1, 
                             max = 10, 
                             step = .1,
                             value = 2.1) %>% 
                  tipify(title = "This input defines how lond it takes for a passenger to board and alight behavior for all buses and routes."), 
                numericInput("pass_alight_sd",
                             "Duration SD:", 
                             min = 0, 
                             max = 10, 
                             step = 1,
                             value = 0) %>% 
                  tipify(title = "Expected standard deviation in boarding time per passenger. Standard deviation uses a normal distribution centered around zero (no delay, or early or late arrival). A zero standard deviation result in no deviation from the mean."),
                actionButton("dist_alight", label = icon("eye")),
                tags$style(type='text/css', "#dist_alight {  margin-top: 25px;}")
    ),
    strong("Bus berth exit condition:"),
    splitLayout(cellWidths = c("100%"), 
                selectInput("exit_cond_status", "Select condition:", 
                            choices = c("Free Exit" = 1, 
                                        "Obstructed" = 2,
                                        "Signal" = 3, 
                                        "Merge" = 4),
                            selected = "Signal") %>%
                  tipify(title = "This input determines the type of exit penalty each bus existing a given stop will experience. Free Exit - no delay penatly, signal - buses may have to wait to exit a berth given downstream dignal timing, merge - bus will have to wait to exit until a sufficiently large headway exists for the bus to occupy.")
    ),
    conditionalPanel(
      condition = "input.exit_cond_status == '3'",
      splitLayout(cellWidths = c("50%", "50%"),
                  numericInput("exit_cond_status_cycle_time",
                               "Cycle Time (sec):", 
                               min = 20, 
                               max = 300, 
                               step = 20,
                               value = 100),
                  numericInput("exit_cond_red_ratio",
                               "Red Ratio:", 
                               min = .1, 
                               max = .9, 
                               step = .1,
                               value = .5)
      )
    ),
    conditionalPanel(
      condition = "input.exit_cond_status == '4'",
      splitLayout(cellWidths = c("50%", "50%"),
                  numericInput("exit_cond_veh_flow",
                               "Vehicle Flow:", 
                               min = 200, 
                               max = 2000, 
                               step = 100,
                               value = 1000),
                  numericInput("exit_cond_sat_flow",
                               "Saturation Flow:", 
                               min = 200, 
                               max = 2000, 
                               step = 100,
                               value = 1900)
      )
    )
  )
}
    
#' sim_inputs_noBox Server Functions
#'
#' @noRd 
mod_sim_inputs_noBox_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_sim_inputs_noBox_ui("sim_inputs_noBox_ui_1")
    
## To be copied in the server
# mod_sim_inputs_noBox_server("sim_inputs_noBox_ui_1")
