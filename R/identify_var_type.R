#' Identify variable types for outcome and predictor
#'
#' Determines whether the outcome and predictor variables are numeric,
#' categorical, or other. This function is designed to be used inside the
#' main workflow where the user supplies a dataset and specifies which
#' variables are the outcome and predictor.
#'
#' @param data A data frame containing the variables.
#' @param outcome A string giving the name of the outcome variable.
#' @param predictor A string giving the name of the predictor variable.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{outcome_type}{Character string: "numeric", "categorical", or "other"}
#'   \item{predictor_type}{Character string: "numeric", "categorical", or "other"}
#' }
#'
#' @examples
#' identify_var_type(PlantGrowth, outcome = "weight", predictor = "group")
#'
#' @export
identify_var_type <- function(data, outcome, predictor) {

  # internal helper to classify a single variable
  classify <- function(x) {
    if (is.numeric(x)) {
      "numeric"
    } else if (is.factor(x) || is.character(x)) {
      "categorical"
    } else {
      "other"
    }
  }

  # extract variables
  out_var <- data[[outcome]]
  pred_var <- data[[predictor]]

  # return types
  list(
    outcome_type = classify(out_var),
    predictor_type = classify(pred_var)
  )
}
