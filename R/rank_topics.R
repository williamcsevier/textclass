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
