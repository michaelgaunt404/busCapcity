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
    
    #===========================================================================
    # List the first level UI elements here
    dashboardPagePlus(
      # introjsUI(), #enables js intro turorial
      
      #header==============================================================================================================================================================================
      header = dashboardHeaderPlus(
        title = "Task Order Scoping Tool",
        titleWidth = 300,
        enable_rightsidebar = T,
        rightSidebarIcon = "gears"
      ),
      #footer==============================================================================================================================================================================
      footer = dashboardFooter(
        left = "By Michael Gaunt",
        right = "Seattle, 2021"
      ),
      #sidebar==============================================================================================================================================================================
      sidebar = dashboardSidebar(width  = 200,
                                 sidebarMenu(id = "tabs",
                                             menuItem("Dashboard", 
                                                      tabName = "db", 
                                                      icon = icon("toolbox"),
                                                      startExpanded = T,
                                                      selected = T),
                                             menuItem("Place Holder 1", 
                                                      tabName = "ph_1", 
                                                      icon = icon("stream"),
                                                      startExpanded = F),
                                             menuItem("Place Holder 2", 
                                                      tabName = "ph_2", 
                                                      icon = icon("table"),
                                                      startExpanded = F)
                                 )
      ),
      #body==============================================================================================================================================================================
      body = dashboardBody(
        tabItems(
          #scope_db tab=========================================================
          tabItem("db",
                  col_4(
                    mod_simulation_inputs_ui("global_inputs")
                  ),
                  col_8(
                    # mod_bus_inputs_ui("bus_inputs_ui_1")
                    uiOutput("bus")
                  )
          ), 
          #timeline tab=========================================================
          tabItem("ph_1",
                  spacer_row(20)          
          ), 
          #data_center tab======================================================
          tabItem("ph_2 ",
                  spacer_row(20)
          )
        )
      ),
      rightsidebar = rightSidebar(
        background = "dark",
        rightSidebarTabContent(
          id = 1,
          title = "Tab 1",
          icon = "desktop",
          active = TRUE,
          sliderInput(
            "obs",
            "Number of observations:",
            min = 0, max = 1000, value = 500
          )
        ),
        rightSidebarTabContent(
          id = 2,
          title = "Tab 2",
          textInput("caption", "Caption", "Data Summary")
        ),
        rightSidebarTabContent(
          id = 3,
          icon = "paint-brush",
          title = "Tab 3",
          numericInput("obs", "Observations:", 10, min = 1, max = 100)
        )
      ),
      title = "Right Sidebar"
    )
    #===========================================================================
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

