# Module UI
  
#' @title   mod_general_overview_page_ui and mod_general_overview_page_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_general_overview_page
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_general_overview_page_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
    mod_stories_table_ui(ns("stories_table")),
    ),
    fluidRow(
      mod_comments_table_ui(ns("comments_table"))
    ),
  )
}
    
# Module Server
    
#' @rdname mod_general_overview_page
#' @export
#' @keywords internal
#' 
#' @importFrom future future value
#' @importFrom hackeRnews get_comments get_top_stories
    
mod_general_overview_page_server <- function(input, output, session) {
  ns <- session$ns
  
  top_stories_promise <- reactiveVal()
  comments_promise <- reactiveVal()
  data_fetching_interval_event <- reactiveTimer(DATA_FETCHING_INTERVAL)
  
  observe({
    data_fetching_interval_event()
    top_stories_promise(future(get_top_stories(max_items = 10)))
  })
  
  observe({
    req(stories_table$selected_story())
    req(top_stories_promise())
    
    top_stories <- value(top_stories_promise())
    selected_top_story <- top_stories[[stories_table$selected_story()]]
    comments_promise(future(get_comments(selected_top_story)))
  })
  
  stories_table <- callModule(mod_stories_table_server, "stories_table", stories_promise = top_stories_promise)
  comments_table <- callModule(mod_comments_table_server, "comments_table", comments_promise = comments_promise)
}