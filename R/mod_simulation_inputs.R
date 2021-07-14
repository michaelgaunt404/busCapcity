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
    box(title = "Simulation Inputs", 
            # closable = F, 
            collapsed = F, 
            collapsible = T, 
            width = "100%", 
            solidHeader = T, 
            status = "primary",
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
    
    
  )
  
}

#' simulation_inputs Server Function
#'
#' @noRd 
mod_simulation_inputs_server <- function(input, output, session){
  ns <- session$ns
  
  
  
  # observeEvent(input$bus_simulation_go, {
  #   showNotification("Input received! Simulations are underway.")
  # })
  # 
  # observeEvent(input$dist_board, {
  #   showNotification("Check out the distributions")
  # })
  # 
  # observeEvent(input$dist_alight, {
  #   showNotification("Check out the distributions")
  # })
  # 
  
 
}
    



# mod_simulation_inputs_ui <- function(id){
#   ns <- NS(id)
#   tagList(
#     # boxPlus_common("Simulation Inputs", 
#     boxPlus(title = "Simulation Inputs", 
#             closable = F, 
#             collapsed = F, 
#             collapsible = T, 
#             width = "100%", 
#             solidHeader = T, 
#             status = "primary",
#             splitLayout(cellWidths = c("30%", "30%", "30%"),
#                         sliderInput("simul_duration",
#                                     "Simulation Duration (min):", 
#                                     min = 30, 
#                                     max = 240, 
#                                     step = 15,
#                                     value = 60),
#                         selectInput("simul_num_routes",
#                                     "Routes to simulate:", 
#                                     choices = c(1:4),
#                                     selected = 1), 
#                         selectInput("simul_num_berths",
#                                     "Berths at stop:", 
#                                     choices = c(1:3),
#                                     selected = 1)
#             ),
#             strong("Passeger board/alight times (sec):"),
#             hr(),
#             splitLayout(cellWidths = c("40%", "40%", "20%"), 
#                         sliderInput("pass_board",
#                                     "Avg Duration:", 
#                                     min = 1, 
#                                     max = 10, 
#                                     step = .1,
#                                     value = 2.1), 
#                         sliderInput("pass_board_sd",
#                                     "Std. Deviation:", 
#                                     min = 0, 
#                                     max = 10, 
#                                     step = 1,
#                                     value = 0),
#                         actionButton("info_glbl_pass", "Info", icon = icon("question-circle"), style = "margin-top: 25px")
#             ),
#             strong("Bus berth exit condition:"),
#             hr(),
#             splitLayout(cellWidths = c("30%", "20%"), 
#                         selectInput("exit_cond_status", "Select condition:", 
#                                     choices = c("Free Exit" = 1, 
#                                                 "Obstructed" = 2, 
#                                                 "Signal" = 3, 
#                                                 "Merge" = 4),
#                                     selected = "Signal"),
#                         actionButton("info_exit_cond", "Info", icon = icon("question-circle"), style = "margin-top: 25px")
#                         # bsButton("q1", label = "click me", icon = icon("question"), style = "margin-top: 30px", size = "extra-small")
#                         #i'd like tpo implement this bs option at some point
#             ),
#             conditionalPanel(
#               condition = "input.exit_cond_status == '3'",
#               splitLayout(cellWidths = c("50%", "50%"),
#                           sliderInput("exit_cond_status_cycle_time",
#                                       "Cycle Time (sec):", 
#                                       min = 20, 
#                                       max = 300, 
#                                       step = 20,
#                                       value = 100),
#                           sliderInput("exit_cond_red_ratio",
#                                       "Red Ratio:", 
#                                       min = .1, 
#                                       max = .9, 
#                                       step = .1,
#                                       value = .5)
#               )
#             ),
#             conditionalPanel(
#               condition = "input.exit_cond_status == '4'",
#               splitLayout(cellWidths = c("50%", "50%"), 
#                           sliderInput("exit_cond_veh_flow",
#                                       "Vehicle Flow:", 
#                                       min = 200, 
#                                       max = 2000, 
#                                       step = 100,
#                                       value = 1000),
#                           sliderInput("exit_cond_sat_flow",
#                                       "Saturation Flow:", 
#                                       min = 200, 
#                                       max = 2000, 
#                                       step = 100,
#                                       value = 1900)
#               )
#             )
#             
#     )
#     
#     
#   )
#   
# }