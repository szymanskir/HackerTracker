#' @import shiny
#' @import shinydashboard
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    dashboardPage(
      dashboardHeader(title = span(tagList(icon("desktop"), "Hacker Tracker"))),
      dashboardSidebar(disable = TRUE),
      dashboardBody(
        fluidPage(
          mod_general_overview_page_ui("general_page")
        )
      )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function() {
  addResourcePath(
    'www', system.file('app/www', package = 'HackerTracker')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    tags$link(rel="stylesheet", type="text/css", href="www/styles.css")
  )
  
}
