#' bus_inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' 
mod_bus_inputs_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyFeedback::useShinyFeedback(),
    uiOutput(ns("bus_inputs"))
  )
}

#REMINDER 
#there are a bunch of action buttons that are intended to let the user view what the distributions look like
#they are currently commented out right now and have space alloted for the in each split layout
#when I tried to uncomment out their repsective events something happend and made them not work

#' bus_inputs Server Function
#'
#' @noRd
mod_bus_inputs_server <- function(input, output, session, route_num){
  ns <- session$ns
  
  output$bus_inputs <- renderUI({
    box(
      width = 4,
      style = "background:#F5F5F5",
      splitLayout(cellWidths = c("50%", "50%"),
                  textInput(inputId = paste0("bus_line_", route_num), label = "Route Name:",
                            value = paste0("bus_route_", route_num)
                  ) %>%  
                    tipify(title = "Please provide a unique bus route name, a placeholder has been provided."),
                  awesomeRadio(inputId = paste0("bus_route_door_cond_", route_num),
                              "Door Condition:",
                              choices = c("Series",
                                          "Parallel"),
                              inline = F,
                              selected = "Series") %>%  
                    tipify(title = "Defines boarding and alighting behavior for buses for this route. Serial indicates alighting operation must finish before boarding operation. Parallel indicates that alighting and boarding operations can occur simultaneously.")            
      ),
      strong("Route Capacity Inputs"),
      splitLayout(cellWidths = c("45%", "40%", "15%"),
                  numericInput(inputId  = paste0("bus_route_size_", route_num), label = "Max Capacity (count):", min = 30, max = 150, step = 1, value = 90) %>%  
                    tipify(title = "Enter maximum passenger capacity for the buses for this route."),
                  numericInput(inputId  = paste0("bus_route_cap_", route_num), label = "% Capacity Available", min = 0, max = 100, step = 1, value = 50) %>% 
                    tipify(title = "Enter percentage of spare capacity for the buses for this route."),
                  actionButton(inputId  = paste0("bus_size_", route_num), label = icon("eye")),
                  tags$style(type='text/css', str_glue("#bus_size_{route_num} {{margin-top: 25px;}}"))
      ),
      strong("Route Inputs"),
      splitLayout(cellWidths = c("45%", "40%", "15%"),
                  numericInput(inputId  = paste0("bus_route_headway_", route_num) , label = "Headway (min)", min = 1, max = 60*6, step = 1, value = 15)  %>% 
                    tipify(title = "Headway indicates how often a bus is scheduled to arrive at the stop."),
                  numericInput(inputId  = paste0("bus_route_headway_sd_", route_num) , label = "Headway SD (min)", min = 0, max = 30, step = 1, value = 3) %>% 
                    tipify(title = "This standard deviation value is used to calculate a random normal value which is applied to each theoretical bus arrival. Uses normal distribution - centered around zero (no delay or early arrival), a zero standard deviation will result in no variance in bus arrivals."),
                  actionButton(inputId  = paste0("dist_headway_", route_num), label = icon("eye")),
                  tags$style(type='text/css', str_glue("#dist_headway_{route_num} {{margin-top: 25px;}}"))
      ),
      strong("Route Passenger Boarding Inputs"),
      splitLayout(cellWidths = c("45%", "40%", "15%"),
                  numericInput(inputId  = paste0("bus_route_pass_", route_num) , label = "Pass. per hour:", min = 0, max = 500, step = 10, value = 40) %>% 
                    tipify(title = "The number of passengers (boarding) serviced by each route per hour. Please input <strong>1</strong> if you wish to model a bus route that does not pick up any passengers."),
                  numericInput(inputId  = paste0("bus_route_pass_sd_", route_num) , label = "Pass. Arrvl. SD (sec):", min = 0, max = 500, step = 1, value = 120) %>% 
                    tipify('<p>This standard deviation value is used to calculate a random normal value which is applied to each theoretical passenger arrival.</p> <p>Uses normal distribution - centered around zero (no delay or early arrival), a zero standard deviation will result in uniform arrival with a constant rate.</p>'),
                  actionButton(inputId  = paste0("dist_route_pass_", route_num), label = icon("eye")),
                  tags$style(type='text/css', str_glue("#dist_route_pass_{route_num} {{margin-top: 25px;}}"))
      ),
      strong("Route Passenger Alighting Inputs"),
      splitLayout(cellWidths = c("45%", "40%", "15%"),
                  numericInput(inputId  = paste0("bus_route_num_alight_", route_num), label = "Alight (%)", min = 0, max = 100, step = 1, value = 20) %>% 
                    tipify(title = "Percentage of total passengers on board that will alight at this stop"),
                  numericInput(inputId  = paste0("bus_route_num_alight_sd_", route_num), label = "Alight sd. (%)", min = 0, max = 10, step = 1, value = 3) %>% 
                    tipify(title = "This standard deviation value is used to calculate a random normal value which is applied to each theoretical bus arrival. Uses normal distribution - centered around zero (no delay or early arrival), a zero standard deviation will result in no variance in bus arrivals."),
                  actionButton(inputId  = paste0("dist_route_num_alight_", route_num), label = icon("eye")),
                  tags$style(type='text/css', str_glue("#dist_route_num_alight_{route_num} {{margin-top: 25px;}}"))
      )
    )
  })

  # observeEvent(input[[paste0("dist_headway_", route_num)]], {
  #   sendSweetAlert(session = session, title = NULL, html = TRUE, btn_labels = c('Close'), text =
  #                    tags$span(style = 'text-align: left;',
  #                              tags$div(id = 'contact_table', renderPlot(
  #                                make_density(
  #                                  90,5
  #                                  # input[[paste0("bus_route_headway_", route_num)]],
  #                                  # input[[paste0("bus_route_headway_sd_", route_num)]]
  #                                  ))
  #                              ))
  #   )}
  # )
  
  
  
  

  # observeEvent( input[[paste0("bus_route_cap_", route_num)]] , {
  #   if ( input[[paste0("bus_route_cap_", route_num)]] == 0 ) {
  #     showFeedbackWarning(
  #       inputId =  paste0("bus_route_cap_", route_num),
  #       text = "fdbck"
  #     )
  #   } else {
  #     hideFeedback(
  #       paste0("bus_route_cap_", route_num)
  #     )
  #   }
  # })
  # 
  # 
  # 
  # observeEvent( input[[paste0("button",i)]] , {
  #   
  #   if ( input[[paste0("button",i)]] < 4 ) {
  #     showFeedbackWarning(
  #       inputId =  paste0("button",i),
  #       text = "fdbck"
  #     )
  #   } else {
  #     hideFeedback(
  #       paste0("button",i)
  #       )
  #     
  #   }
  # })
  
  
}






