#' Normalize Optimal Topic Metrics
#'
#'This function allows you to normalize the output from optimal topic analysis
#' @param values optimal topics results
#' @keywords normalize
#' @export
#' @examples
#' normalize_metrics()
normalize_metrics <- function(values) {
  # normalize to [0,1]
  columns <- base::subset(values, select = 2:ncol(values))
  values <- base::data.frame(
    values["topics"],
    base::apply(columns, 2, function(column) {
      scales::rescale(column, to = c(0, 1), from = range(column))
    })
  )
  return(values) }
