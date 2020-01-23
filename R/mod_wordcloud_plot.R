# Module UI
  
#' @title   mod_wordcloud_plot_ui and mod_wordcloud_plot_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_wordcloud_plot
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @importFrom shinydashboard box
#' @importFrom shinycssloaders withSpinner
#' 
mod_wordcloud_plot_ui <- function(id, title){
  ns <- NS(id)
  tagList(
    box(
      withSpinner(plotOutput(ns("wordcloud_plot"))),
      title = span(tagList(icon("cloud"), title)),
      width = NULL,
      solidHeader = TRUE,
      collapsible = TRUE,
      status = "primary"
    )
  )
}
    
# Module Server
    
#' @rdname mod_wordcloud_plot
#' @export
#' @keywords internal
#' @import ggplot2
#' @import ggwordcloud
    
mod_wordcloud_plot_server <- function(input, output, session, comments_promise) {
  ns <- session$ns
  
  output$wordcloud_plot <- renderPlot({
    req(comments_promise())
    comments_promise() %...>%
      pull(text) %...>%
      calculate_word_frequencies() %...>%
      arrange(Freq) %...>%
      filter(Freq > 1) %...>%
      top_n(42) %...>%
      mutate(color = factor(sample(10, length(words), replace = TRUE))) %...>% {
        ggplot(., aes(label = words, size = Freq, color = color)) + 
          geom_text_wordcloud(rm_outside = TRUE) +
          scale_size_area(max_size = 18)
    }
  })
}
    
## To be copied in the UI
# mod_wordcloud_plot_ui("wordcloud_plot_ui_1")
    
## To be copied in the server
# callModule(mod_wordcloud_plot_server, "wordcloud_plot_ui_1")
 
