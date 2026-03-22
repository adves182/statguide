#' Count the number of groups in a predictor variable
#'
#' Determines how many unique groups (levels or categories) the predictor
#' variable contains. This function is designed to support the decision tree
#' for choosing the appropriate statistical test.
#'
#' @param x A vector (factor or character).
#'
#' @return Integer number of unique groups.
#'
#' @export
count_groups <- function(x) {
  if (!is.factor(x) && !is.character(x)) {
    stop("count_groups() requires a categorical variable.")
  }
  length(unique(x))
}
