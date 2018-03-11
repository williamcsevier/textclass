#' @title N-gram term frequency for AFICA contract data
#'
#' @description This function allows you to analyze n-gram term frequency for AFICA contract descriptions
#' @param data data frame where text has column name "description"
#' @param n number of n-grams for frequency analysis (n=1 for single term frequency)
#' @keywords n-gram
#' @export
#' @examples
#' term_frequency(data, 2)
term_frequency <- function(data,n = 1){
sw <- add_row(stop_words, word = c("igf","ot", "ct"), lexicon = c("SMART", "SMART", "SMART"))

if(n == 2){
plt <- data %>%
  unnest_tokens(word, description, token = "ngrams", n = n) %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% sw$word) %>%
  filter(!word2 %in% sw$word) %>%
  unite("word", word1, word2, sep =" ") %>%
  count( word, sort = TRUE) %>%
  top_n(10,n)
}
else if(n==3){
  plt <- data %>%
    unnest_tokens(word, description, token = "ngrams", n = n) %>%
    separate(word, c("word1", "word2", "word3"), sep = " ") %>%
    filter(!word1 %in% sw$word) %>%
    filter(!word2 %in% sw$word) %>%
    filter(!word3 %in% sw$word) %>%
    unite("word", c(word1, word2, word3), sep =" ") %>%
    count( word, sort = TRUE) %>%
    top_n(10,n)
}
else if(n==4){
  plt <- data %>%
    unnest_tokens(word, description, token = "ngrams", n = n) %>%
    separate(word, c("word1", "word2", "word3", "word4"), sep = " ") %>%
    filter(!word1 %in% sw$word) %>%
    filter(!word2 %in% sw$word) %>%
    filter(!word3 %in% sw$word) %>%
    filter(!word4 %in% sw$word) %>%
    unite("word", c(word1, word2, word3, word4), sep =" ") %>%
    count( word, sort = TRUE) %>%
    top_n(10,n)
}
else if(n>4){
  print("too many n-grams")
}
else{
  plt <- data %>%
    unnest_tokens(word, description) %>%
    anti_join(sw) %>%
    count( word, sort = TRUE) %>%
    top_n(10,n)}

plt <- ggplot(plt, aes(reorder(word, n), n)) +
  geom_bar(stat = "identity") +
  xlab("Term") +
  ylab("Count") +
  coord_flip()
return(plt)
}
