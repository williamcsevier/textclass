#' @title Plots Optimal Topic Analysis from Normalized Optimal Topic Values
#'
#' @description This function allows you to to plot the normalized Cao and Deveaud normalized latent structure values
#' @param values normalized cao and deveaud optimal topics output
#' @keywords plot optimal topics
#' @export
#' @examples
#' plot_optimal_topics(optimal_topics_norm_values)
plot_optimal_topics <- function(values){
plt <-   ggplot(values, aes(x=topics)) +
    labs(title="Structure Analysis per Topic Number", x="Number of Topics", y="Normalized Metric", colour = "Metric") +
    geom_line(aes(y = CaoJuan2009, color = "CaoJuan2009")) +
    geom_line(aes(y = Deveaud2014, color = "Deveaud2014")) +
    scale_x_continuous(breaks = values$topics[seq(1, length(topics_collapsed$topics), by = 1)])
  return(plt)
}
