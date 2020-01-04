#' @import shiny
app_server <- function(input, output, session) {
  future::plan(future::multiprocess)
  # List the first level callModules here
  callModule(module = mod_general_overview_page_server, id = "general_page")
}
