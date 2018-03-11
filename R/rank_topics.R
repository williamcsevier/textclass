#' @title Creates Rank Order of Optimal Topics
#'
#' @description This function allows you to analyze the Cao and Deveaud optimal topics by rank ordering the metrics
#' @param values normalized optimal topic metric output
#' @keywords rank optimal topics
#' @export
#' @examples
#' rank_topics(values)
rank_topics <- function (values){
  topics_collapsed_ranked <- values %>%
    mutate(#"Griffiths" = rank(-Griffiths2004),
      "Cao" = rank(CaoJuan2009),
      "Deveaud" = rank(-Deveaud2014)) %>%
    mutate(Sum = rowSums(select_(., "Cao", "Deveaud"))) %>%
    mutate(Rank = as.integer(rank(Sum)))

  optimal_topics_ranked <- topics_collapsed_ranked %>%
    dplyr::select(topics, Griffiths2004, CaoJuan2009, Arun2010, Deveaud2014,Rank) %>%
    top_n(5, -Rank) %>%
    arrange(Rank)

  return(optimal_topics_ranked)
}
