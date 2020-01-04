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
#' @import tidyr
#' @import tidytext
#' 
calculate_sentiment <- function(sentences) {
  sentiment_index <- get_sentiments("bing")
  
  calculate_single_sentence_sentiment <- function(sentence) {
    words <- sentence %>% 
      clean_sentences() %>% 
      remove_stop_words()
    
    words_df <- data.frame(word = words, stringsAsFactors = FALSE)
    words_df %>% 
      inner_join(sentiment_index, by = "word") %>% 
      mutate(sentiment = factor(sentiment, levels = c("positive", "negative"))) %>% 
      count(word, sentiment, .drop = FALSE) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment_score = positive - negative) %>%
      summarize(total_sentiment_score = sum(sentiment_score)) %>% 
      pull(total_sentiment_score)
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
calculate_word_frequencies <- function(sentences) {
  words <-  sentences %>% 
    clean_sentences() %>% 
    remove_stop_words()
  
  table(words) %>% 
    as.data.frame()
}


#' Cleans input senteces from html, converts to lower
#' characters and simplifies white spaces.
#' 
#' @param sentences vector of input sentences
#' 
#' @return vector of words 
#' 
#' @import stringr
#' 
clean_sentences <- function(sentences) {
  sentences %>% 
    remove_html() %>% 
    str_to_lower() %>%
    str_replace_all('[^A-Z|a-z]', ' ') %>% 
    str_replace_all('\\s\\s*', ' ') %>% 
    str_split(' ', simplify = TRUE)
}

#' Removes html elements from the input string
#' 
#' @param texts vector of input string to remove html from
#' 
#' @return vector of strings with removed html elements
#' 
#' @importFrom rvest html_text
#' @importFrom xml2 read_html
#' 
remove_html <- function(texts) {
  html_strings <- sprintf("<body>%s<body>", texts)
  sapply(html_strings, function(html_string) {
    html_string %>% 
      read_html() %>% 
      html_text()
  }, USE.NAMES = FALSE)
}


#' Removes stop words from the given vector of words
#' 
#' @param words vector of words from which stop words
#' should be removed
#' 
#' @return vector of words with stop_words removed
#' 
remove_stop_words <- function(words) {
  stop_words <- tidytext::stop_words
  words[!words %in% c("", " ", stop_words$word)]
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