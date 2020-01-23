# Module UI
  
#' @title   mod_sentiment_distribution_plot_ui and mod_sentiment_distribution_plot_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_sentiment_distribution_plot
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @importFrom shinydashboard box
#' @importFrom shinycssloaders withSpinner
#' 
mod_sentiment_distribution_plot_ui <- function(id, title) {
  ns <- NS(id)
  tagList(
    box(
      withSpinner(plotOutput(ns("sentiment_plot"))),
      title = span(tagList(icon("chart-area"), title)),
      width = NULL,
      solidHeader = TRUE,
      collapsible = TRUE,
      status = "primary"
    )
  )
}
    
# Module Server
    
#' @rdname mod_sentiment_distribution_plot
#' @export
#' @keywords internal
#' @import ggplot2
#' 
mod_sentiment_distribution_plot_server <- function(input, output, session, comments_promise) {
  ns <- session$ns
  
  output$sentiment_plot <- renderPlot({
    req(comments_promise())
    comments_promise() %...>% {
        comments <- .
        validate(need(!is.null(comments), "Unfortunately there are no comments for this story."))
        comments %>% pull(text)
      } %...>% {
          ggplot(., aes(x = sentiment)) + 
            geom_density(alpha = 0.5, fill = "lightblue") +
            xlim(-10, 10)
        }
  })
}
    
## To be copied in the UI
# mod_sentiment_distribution_plot_ui("sentiment_distribution_plot_ui_1")
    
## To be copied in the server
# callModule(mod_sentiment_distribution_plot_server, "sentiment_distribution_plot_ui_1")
 
