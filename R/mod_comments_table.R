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
      title = span(tagList(icon("comments"), "Comments")),
      solidHeader = TRUE,
      collapsible = TRUE,
      status = "primary",
      width = NULL
    )
  )
}
    
# Module Server
    
#' @rdname mod_comments_table
#' @export
#' @keywords internal
#' 
#' @import dplyr
#' @import stringr
    
mod_comments_table_server <- function(input, output, session, comments_promise) {
  ns <- session$ns
  
  output$comments_table <- DT::renderDataTable({
    req(comments_promise())
    
    comments_promise() %...>%
      mutate(text = str_trunc(text, 80)) %...>%
      select(time, by, sentiment) %...>% 
      DT::datatable(selection = "single")
  })
  
  list(
    selected_row = reactive({input$comments_table_row_last_clicked})
  )
}