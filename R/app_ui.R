#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    useShinyFeedback(), #needed for input feedback
    
    #===========================================================================
    # List the first level UI elements here
    dashboardPage(
      # introjsUI(), #enables js intro turorial
      
      # shinybusy::add_busy_bar(color = "#FF0000"),
      #header==============================================================================================================================================================================
      header = dashboardHeader(
        title = "Bus Capacity Modeler",
        titleWidth = 300,
        tags$li(class = 'dropdown',
                actionLink('contact', label = '', icon = icon('envelope-o', class = 'fa-lg'))),
        tags$li(class = 'dropdown',
                tags$a(href = "https://www.wsp.com/en-GL", target = "_blank",
                       tags$img(height = "20px", alt = "WSP USA ©", src = "www/wsp_logo.png")))
      ),
      #footer==============================================================================================================================================================================
      # footer = dashboardFooter(
      #   left = "By Michael Gaunt",
      #   right = "Seattle, 2021"
      # ), #this has been messing up and the footer ends up everywhere
      #sidebar==============================================================================================================================================================================
      sidebar = dashboardSidebar(width  = 300,
                                 sidebarMenu(id = "tabs",
                                             menuItem("Simulation Setup"
                                                      # ,tabName = "db"
                                                      ,icon = icon("toolbox")
                                                      ,startExpanded = T
                                                      # ,selected = T
                                                      ,menuSubItem("Define Inputs", tabName = "db")
                                                      ,menuSubItem("View Initialized Inputs", tabName = "simul_initial")
                                                      
                                                      # bus_input_go_2
                                                      
                                                      ,mod_input_validate_ui("input_validate_ui_1")
                                                      
                                                      ,actionButton("bus_input_go", "View Bus Inputs") %>%  
                                                        tipify(title = "Step 1: Click this button to review the inputs you have provided below.")
                                                      ,actionButton("bus_simulation_go", "Run Simulation") %>%
                                                        tipify(title = "Step 2: If you have reviewed your inputs, click this button to intiate the simulations.")
                                                      ,hr()
                                                      # ,mod_sim_inputs_noBox_ui("global_inputs")
                                                      
                                             )
                                             ,menuItem("Simulation Results"
                                                       ,icon = icon("stream"),startExpanded = F
                                                       ,menuSubItem("Old Tab", tabName = "simul_result")
                                                       ,menuSubItem("Visualization", tabName = "results_vis")
                                                       ,menuSubItem("Raw Data", tabName = "results_raw_data")
                                             )
                                             ,menuItem("Debug and Dev", 
                                                       tabName = "debug_dev", 
                                                       icon = icon("bug"),
                                                       startExpanded = F,
                                                       mod_get_variables_ui("get_variables_ui_1")
                                             )
                                             ,menuItem("Glosary", 
                                                       tabName = "glos", 
                                                       icon = icon("book"),
                                                       startExpanded = F
                                             )
                                 )
      ),
      #body==============================================================================================================================================================================
      body = dashboardBody(
        tabItems(
          #scope_db tab=========================================================
          tabItem("db",
                  col_3(
                    mod_simulation_inputs_ui("global_inputs")
                  ),
                  col_9(
                    box_common(
                      title = "Bus Route Inputs",
                      uiOutput("bus")
                    )
                    # ,splitLayout(
                    #   cellWidths = c("50%", "50%"),
                    #   box_common(title = "Bus Route Input Summary",
                    #              collapsed = T,
                    #              list(
                    #                "This table details simulated buses given user provided inputs.",
                    #                br(),
                    #                "It is repersentative of each simulation if more than one simulation is ran.",
                    #                br(),
                    #                DT::dataTableOutput("smmry_bus_routes") %>%  withSpinner()
                    #              )
                    #   ), 
                    #   tabBox(
                    #     width = "100%",
                    #     tabPanel(
                    #       "Quick Statistics", 
                    #       plotlyOutput("pass_arrvl_cumm") %>%  withSpinner()
                    #     ),
                    #     tabPanel(
                    #       "Quick Statistics", 
                    #       plotlyOutput("pass_arrvl_hist") %>%  withSpinner()
                    #     )
                    #   )
                    # )
                  ) 
          ), 
          #SIM: Initialization==================================================
          tabItem("simul_initial",
                  col_6(
                    box_common(title = "Bus Route Input Summary",
                               collapsed = F,
                               list(
                                 "This table details simulated buses given user provided inputs.",
                                 br(),
                                 "It is repersentative of each simulation if more than one simulation is ran.",
                                 br(),
                                 DT::dataTableOutput("smmry_bus_routes") %>%  withSpinner()
                               )
                    )
                  )
                  ,col_6(
                    tabBox(
                      width = "100%",
                      tabPanel(
                        "Cummlative Arrival Plot",
                        plotlyOutput("pass_arrvl_cumm") %>%  withSpinner()
                      ),
                      tabPanel(
                        "Arrival Distribution Plot",
                        plotlyOutput("pass_arrvl_hist") %>%  withSpinner()
                      )
                    )
                  )
          ),
          #RESULTS: base tab tab==================================================
          tabItem("simul_result"
                  ,col_4(
                    # box_common(
                    #   title = "Aggregated Simulation Metrics", 
                    #   DT::dataTableOutput("results_summary_stats") %>%  
                    #     withSpinner()
                    # )
                  )
                  ,col_8(
                    # mod_output_dt_ui("summary_tab") 
                  )        
          ),
          #RESULTS: temp VIS tab================================================
          tabItem("results_vis"
                  ,col_4(
                    box_common(
                      title = "Aggregated Simulation Metrics", 
                      DT::dataTableOutput("results_summary_stats") %>%  
                        withSpinner()
                    )
                  )
                  ,col_8(
                    mod_result_viz_ui("yolo_check")
                  )
                  # ,mod_result_viz_ui("yolo_check")
          ),
          #RESULTS: temp RAW_Data tab===========================================
          tabItem("results_raw_data"
                  ,mod_output_dt_ui("summary_tab")
          ),
          #glossary tab=========================================================
          tabItem("glos",
                  mod_glassary_tab_ui("glassary_tab_ui_1")
          )
          
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'busCapacity'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

