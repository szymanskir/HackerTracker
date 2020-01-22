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
#' @importFrom shiny NS tagList selectInput actionLink
#' @importFrom shinydashboard box
#' @importFrom shinycssloaders withSpinner
mod_stories_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      actionLink(ns("help"), "Start here!"),
      selectInput(
        ns("story_type"),
        "Select stories type:",
        choices = c('Top stories' = 'top', 'Best stories' = 'best', 'New Stories' = 'new'),
        selected = 'top'
      ),
      rintrojs::introBox(
        withSpinner(
          DT::dataTableOutput(ns("stories_table"))
        ),
        data.step = 1,
        data.intro = "Select row to see statistics.",
        data.position = "right"
      ),
      title = span(tagList(icon("newspaper"), "Stories")),
      solidHeader = TRUE,
      collapsible = TRUE,
      status = "primary",
      width = NULL
    )
  )
}
    
# Module Server
    
#' @rdname mod_stories_table
#' @export
#' @keywords internal
#' 
#' @import promises
#' @importFrom hackeRnews get_comments get_top_stories get_new_stories get_best_stories

    
mod_stories_table_server <- function(input, output, session) {
  ns <- session$ns
  
  dataSource <- reactiveVal()
  selected_story_r <- reactiveVal()
  
  observeEvent(input$story_type, {
    req(input$story_type)
    selected_story_r(NULL)
    max_items <- 10
    if (input$story_type == "top") {
      dataSource(future(get_top_stories(max_items = max_items)))
    }
    else if (input$story_type == "best") {
      dataSource(future(get_best_stories(max_items = max_items)))
    }
    else if (input$story_type == "new") {
      dataSource(future(get_new_stories(max_items = max_items)))
    }
  })
  
  observeEvent(input$stories_table_row_last_clicked, {
    stories <- value(dataSource())
    selected_story_r(stories[[input$stories_table_row_last_clicked]])
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  
  output$stories_table <- DT::renderDataTable({
    req(dataSource())
    dataSource() %...>%
      lapply(function(item) {
        data.frame(
          title = item$title,
          by = item$by,
          score = item$score
        )
      }) %...>%
      do.call(rbind, .) %...>%
      DT::datatable(selection = "single", options = list(lengthChange = FALSE, pageLength = 5))
  })
  
  observeEvent(input$help, {
    rintrojs::introjs(session)
  })
  
  list(
    selected_story = reactive({ selected_story_r()})
  )
}
