#' @title Plots Topic Models from Latent Dirichlet Allocation Model
#'
#' @description This function allows you to analyze topic models from an Latent Dirichlet Allocation model
#' @param lda latent dirichlet allocation model
#' @keywords lda topic model
#' @export
#' @examples
#' plot_topics(lda)
plot_topics <- function(lda) {
topics <- tidy(lda, matrix = "beta")
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup()

top_terms <- top_terms %>%
  ungroup() %>%
  arrange(topic, beta) %>%
  mutate(.r = row_number())

plt <- ggplot(top_terms,aes(.r, beta,
                     fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_x_continuous(
    breaks = top_terms$.r,
    labels = top_terms$term)+
  xlab("term")+
  coord_flip()

return(plt)
}
