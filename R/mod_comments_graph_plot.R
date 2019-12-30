# Module UI
  
#' @title   mod_comments_graph_plot_ui and mod_comments_graph_plot_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_comments_graph_plot
#'
#' @keywords internal
#' @export 
#' @import sigmajs
#' @importFrom shiny NS tagList 
#' @importFrom shinydashboard box
#' @importFrom shinycssloaders withSpinner
mod_comments_graph_plot_ui <- function(id, title) {
  ns <- NS(id)
  tagList(
    box(
      withSpinner(
        sigmajsOutput(ns("comments_graph_plot"))
      ),
      title = span(tagList(icon("project-diagram"), title)),
      solidHeader = TRUE,
      collapsible = TRUE,
      width = NULL,
      status = "primary"
    )  
  )
}
    
# Module Server
    
#' @rdname mod_comments_graph_plot
#' @export
#' @keywords internal
#' 
#' @import promises
#' @import sigmajs
    
mod_comments_graph_plot_server <- function(input, output, session, comments_promise, selected_comment) {
  ns <- session$ns
  
  observe({
    req(selected_comment())
    req(comments_promise())
    
    comments_promise() %...>% {
      graph_data <- comments_to_graph_data(.)
      graph_data$nodes$color[selected_comment()] <- "#00FF00"
      sigmajsProxy(ns("comments_graph_plot")) %>% 
        sg_zoom_p(id = selected_comment() + 1, duration = 2000, ratio = 0.5) %>% 
        sg_change_nodes_p(graph_data$nodes, color, "color")
    }
    
  })
  
  output$comments_graph_plot <- sigmajs::renderSigmajs({
    req(comments_promise())
    
    comments_promise() %...>% {
      comments <- .
      graph_data <- comments_to_graph_data(comments)
      
      sigmajs() %>% 
        sg_nodes(graph_data$nodes, id, label, size, color) %>% 
        sg_edges(graph_data$edges, id, source, target) %>% 
        sg_layout(layout = igraph::layout_as_tree) %>% 
        sg_events("hoverNode") %>% 
        sg_settings(
          labelThreshold = 100,
          edgeColor = "default",
          defaultEdgeColor = "black",
          nodeHoverColor = "default",
          defaultNodeHoverColor = "#00FF00",
          singleHover = TRUE
        )
    }
  })
  
  list(
    hovered_node = reactive({input$comments_graph_plot_over_node})
  )
}
