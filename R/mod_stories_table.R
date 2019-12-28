# Module UI
  
#' @title   mod_stories_table_ui and mod_stories_table_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_stories_table
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @importFrom shinydashboard box
#' @importFrom shinycssloaders withSpinner
mod_stories_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      withSpinner(
        DT::dataTableOutput(ns("stories_table"))
      ),
      title = "Stories",
      solidHeader = TRUE,
      collapsible = TRUE,
      status = "primary",
      col = 4
    )
  )
}
    
# Module Server
    
#' @rdname mod_stories_table
#' @export
#' @keywords internal
    
mod_stories_table_server <- function(input, output, session, stories_promise) {
  ns <- session$ns
  
  output$stories_table <- DT::renderDataTable({
    req(stories_promise())
    stories_data <- future::value(stories_promise())
    stories_data <- lapply(stories_data, function(item) {
        data.frame(
          title = item$title,
          by = item$by,
          score = item$score
        )
      }
    )
    stories_data <- do.call(rbind, stories_data)
    DT::datatable(stories_data, selection = "single")
  })
  
  list(
    selected_story = reactive({input$stories_table_row_last_clicked})
  )
}