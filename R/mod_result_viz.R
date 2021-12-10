#' result_viz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_result_viz_ui <- function(id){
  ns <- NS(id)
  tagList(
    col_4(
      box(title = "Simulation Module Description" 
          ,closable = F ,collapsed = F ,collapsible = T 
          ,width = "100%" ,solidHeader = T, status = "primary"
          ,plotly::plotlyOutput(ns("viz_boxplot"))
      )
    )
    
  )
}
    
#' result_viz Server Functions
#'
#' @noRd 
mod_result_viz_server <- function(id, .data){
  # 
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$viz_boxplot = plotly::renderPlotly({
      temp = .data()[[3]] %>%
        # sim[[3]]  %>% 
        select(bus_line, bus_line_id, bus_id, index_resultPlot$names_raw) %>% 
        unique() %>%
        pivot_longer(cols = !c(bus_line:bus_id)) %>%  
        merge(., index_resultPlot, by.x = "name", by.y = "names_raw") %>%  
        select(bus_line, bus_line_id, names_p1, value) %>%  
        arrange(bus_line, names_p1)
      
      temp %>%  
        print()
      
      temp %>%  
        plotly::plot_ly(x = ~bus_line, y = ~value, color = ~bus_line,
                        type = "box", boxmean = T,
                        transforms = list(
                          list(type = 'filter', target = ~names_p1, operation = '=',
                               value = unique(temp$names_p1)[1]
                          )
                        )) %>%
        plotly::layout(xaxis = list(title = ""), 
                       yaxis = list(title = ""),
                       updatemenus =
                         list(
                           make_menu_item(name_list = unique(temp$names_p1), filter_pos = 0, type = "buttons",
                                          direction = "down", x = -0.5, y = 1.1)[[1]]
                           
                         ),
                       showlegend = T)
    })
    
  })
}
    
## To be copied in the UI
# mod_result_viz_ui("result_viz_ui_1")
    
## To be copied in the server
# mod_result_viz_server("result_viz_ui_1")
