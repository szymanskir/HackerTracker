#' Calculates the sentiment values for each sentences on by one
#' 
#' @param sentences vector of strings for which the sentiment values
#' should be calculated
#' 
#' @return vector of sentiment values corresponding to the inout
#' sentences
#' 
#' @import dplyr
#' @import stringr
#' @import textdata
#' @import tidytext
calculate_sentiment <- function(sentences) {
  data(stop_words, package = "tidytext")
  sentiment_index <- get_sentiments("afinn")
  
  calculate_single_sentence_sentiment <- function(sentence) {
    words <- sentence %>% 
      str_replace_all('[^A-Z|a-z]', ' ') %>%
      str_to_lower() %>%
      str_replace_all('\\s\\s*', ' ') %>% 
      str_split(' ', simplify = TRUE) %>% 
      setdiff(c("", stop_words))
    
    words_df <- data.frame(word = words, stringsAsFactors = FALSE)
    words_df %>% 
      inner_join(sentiment_index, by = "word") %>% 
      summarise(sentiment_score = mean(value)) %>% 
      pull(sentiment_score)
  }
  
  sapply(sentences, calculate_single_sentence_sentiment, USE.NAMES = FALSE)
}


#' Calculates the global frequencies of words in the given sentences
#' 
#' @param sentences vector of strings for which the word frequencies
#' should be calculated
#' 
#' @return data.frame with word frequencies
#' 
#' @import stringr
#' 
#' @import textdata
#' @import tidytext
calculate_word_frequencies <- function(sentences) {
  data(stop_words, package = "tidytext")
  words <- sentences %>% 
    str_replace_all('[^A-Z|a-z]', ' ') %>%
    str_to_lower() %>%
    str_replace_all('\\s\\s*', ' ') %>% 
    str_split(' ', simplify = TRUE) %>% 
    as.vector()
  
  words <- words[!words %in% c("", stop_words$word)]
  
  table(words) %>% 
    as.data.frame()
}


comments_to_graph_data <- function(comments) {
  color_palette <- grDevices::colorRampPalette(c('red','blue'))
  
  root_node <- setdiff(comments$parent, comments$id) %>% 
    unique()
  
  nodes <- data.frame(
    id = comments$id,
    label = comments$by,
    color = color_palette(10)[as.numeric(cut(comments$sentiment,breaks = 10))],
    stringsAsFactors = FALSE
  )
  nodes$size <- 1
  nodes <- rbind(nodes, data.frame(id = root_node, size = 2, color = -10, label = "root", stringsAsFactors = FALSE))
  
  edges <- data.frame(
    id = seq_len(nrow(comments)),
    source = comments$parent,
    target = comments$id,
    stringsAsFactors = FALSE
  )
  
  list(
    nodes = nodes,
    edges = edges
  )
}