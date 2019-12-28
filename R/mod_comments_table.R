# Module UI
  
#' @title   mod_comments_table_ui and mod_comments_table_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_comments_table
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @importFrom shinydashboard box
#' @importFrom shinycssloaders withSpinner
mod_comments_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      withSpinner(
        DT::dataTableOutput(ns("comments_table"))
      ),
      title = "Comments",
      solidHeader = TRUE,
      collapsible = TRUE,
      status = "primary",
      col = 4
    )
  )
}
    
# Module Server
    
#' @rdname mod_comments_table
#' @export
#' @keywords internal
    
mod_comments_table_server <- function(input, output, session, comments_promise) {
  ns <- session$ns
  
  output$comments_table <- DT::renderDataTable({
    req(comments_promise())
    comments_data <- future::value(comments_promise())
    comments_data$text <- stringr::str_trunc(comments_data$text, 100)
    DT::datatable(comments_data[, c("text", "time", "by")], selection = "single")
  })
  
  list(
    selected_row = reactive({input$comments_table_row_last_clicked})
  )
}