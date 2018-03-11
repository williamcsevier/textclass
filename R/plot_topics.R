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
