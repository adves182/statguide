#' Count the number of groups in a predictor variable
#'
#' Determines how many unique groups (levels or categories) the predictor
#' variable contains. This function is designed to support the decision tree
#' for choosing the appropriate statistical test.
#'
#' @param data A data frame containing the variables.
#' @param predictor A string giving the name of the predictor variable.
#'
#' @return An integer giving the number of unique groups in the predictor.
#'
#' @examples
#' count_groups(PlantGrowth, predictor = "group")
#'
#' @export
count_groups <- function(data, predictor) {

  x <- data[[predictor]]

  # If factor, count levels
  if (is.factor(x)) {
    return(length(levels(x)))
  }

  # If character, count unique values
  if (is.character(x)) {
    return(length(unique(x)))
  }

  # If numeric, treat unique values as groups
  if (is.numeric(x)) {
    return(length(unique(x)))
  }

  # Otherwise, return NA with a warning
  warning("Unsupported variable type for counting groups.")
  return(NA_integer_)
}
