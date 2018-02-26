#' Determine Optimal Topics

optimal_topics <- function(dtm){
  result <- FindTopicsNumber(
    dtm,
    topics = seq(from = 2, to = 30, by = 1),
    metrics = c("CaoJuan2009", "Deveaud2014"),
    method = "Gibbs",
    control = list(seed = 1234),
    mc.cores = 2L, #make sure this is appropriate number of cores you wish to use
    verbose = TRUE
  )
}
