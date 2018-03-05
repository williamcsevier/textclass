#' Constructing DTM for AFICA data
#'
#'This function allows you to analyze document term matrix for latent structures
#' @param text.df text data
#' @param sparse sparsity trimmed
#' @keywords dtm
#' @export
#' @examples
#' tidyDTM()

tidyDTM <- function(text.df, sparse){
  sw <- add_row(stop_words, word = c("igf","ot", "ct"), lexicon = c("SMART", "SMART", "SMART"))

  #word counts
  word_counts <- text.df %>%
    unnest_tokens(word, description) %>%
    anti_join(sw) %>%
    count(document, word, sort = TRUE) %>%
    ungroup()

  #cast dtm
  dtm <- word_counts %>%
    cast_dtm(document, word, n)

  dtm$dimnames$Terms <- gsub("function", "functio", dtm$dimnames$Terms)

  dtmNoSparse <- removeSparseTerms(dtm, sparse)

  return(dtmNoSparse)
}
