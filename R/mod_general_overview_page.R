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
mod_general_overview_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      rintrojs::introjsUI(),
      column(
        width = 4,
        mod_stories_table_ui(ns("stories_table")),
        mod_comments_table_ui(ns("comments_table"))
      ),
      column(
        width = 4,
        mod_sentiment_distribution_plot_ui(
          id = ns("sentiment_plotter"), 
          title = "Sentiment distribution"
        ),
        mod_comments_graph_plot_ui(
          id = ns("comments_graph_plot"),
          title = "Comments graph" 
        )
      ),
      column(
        width = 4,
        mod_wordcloud_plot_ui(
          id = ns("wordcloud_plot"),
          title = "Story buzzwords"
        ),
        box(
          h4(textOutput(ns("hovered_node_text"))),
          title = span(tagList(icon("file-word"), "Hovered node text")),
          width = NULL,
          solidHeader = TRUE,
          collapsible = TRUE,
          status = "primary"
        )
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_general_overview_page
#' @export
#' @keywords internal
#' 
#' 
#' @importFrom future future value
#' @importFrom hackeRnews get_comments get_top_stories
#' 
    
mod_general_overview_page_server <- function(input, output, session) {
  ns <- session$ns
  
  top_stories_promise <- reactiveVal()
  comments_promise <- reactiveVal()
  data_fetching_interval_event <- reactiveTimer(DATA_FETCHING_INTERVAL)
  
  output$hovered_node_text <- renderText({
    req(comments_graph$hovered_node())
    req(comments_promise())
    
    comments_promise() %...>%
      filter(id == comments_graph$hovered_node()$id) %...>%
      pull(text) %...>% 
      remove_html()
  })
  
  observe({
    data_fetching_interval_event()
    top_stories_promise(future(get_top_stories(max_items = 10)))
  })
  
  observe({
    req(stories_table$selected_story())
    req(top_stories_promise())
    
    top_stories <- value(top_stories_promise())
    selected_top_story <- top_stories[[stories_table$selected_story()]]
    comments_with_sentiment_promise <- future(get_comments(selected_top_story)) %...>%
      mutate(sentiment = calculate_sentiment(text) %>% round(digits = 2) %>% min(10) %>% max(-10))
      
    comments_promise(comments_with_sentiment_promise)
  })
  
  # Modules
  stories_table <- callModule(mod_stories_table_server, "stories_table", stories_promise = top_stories_promise)
  comments_table <- callModule(mod_comments_table_server, "comments_table", comments_promise = comments_promise)
  callModule(mod_sentiment_distribution_plot_server, "sentiment_plotter", comments_promise = comments_promise)
  comments_graph <- callModule(
    mod_comments_graph_plot_server, 
    "comments_graph_plot", 
    comments_promise = comments_promise, 
    selected_comment = comments_table$selected_row
  )
  callModule(mod_wordcloud_plot_server, "wordcloud_plot", comments_promise = comments_promise)
}