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
  stop_words <- data("stop_words") 
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