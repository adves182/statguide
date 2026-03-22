#' Choose an appropriate statistical test
#'
#' Based on the variable types (numeric or categorical) and the number of
#' groups in the predictor, this function selects an appropriate statistical
#' test. It uses helper functions \code{identify_var_type()} and
#' \code{count_groups()} to determine the structure of the data.
#'
#' @param data A data frame containing the variables.
#' @param outcome A string giving the name of the outcome variable.
#' @param predictor A string giving the name of the predictor variable.
#'
#' @return A character string naming the recommended statistical test.
#'
#' @examples
#' choose_test(PlantGrowth, outcome = "weight", predictor = "group")
#'
#' @export
choose_test <- function(data, outcome, predictor) {

  # Identify variable types (updated for new identify_var_type())
  outcome_type <- identify_var_type(data[[outcome]])
  predictor_type <- identify_var_type(data[[predictor]])

  # If both numeric → correlation
  if (outcome_type == "numeric" && predictor_type == "numeric") {
    return("correlation")
  }

  # If outcome numeric + predictor categorical → t-test or ANOVA
  if (outcome_type == "numeric" && predictor_type == "categorical") {
    n_groups <- count_groups(data[[predictor]])

    if (n_groups == 2) {
      return("t_test")
    } else if (n_groups > 2) {
      return("anova")
    } else {
      warning("Predictor has fewer than 2 groups.")
      return(NA_character_)
    }
  }

  # If both categorical → chi-square
  if (outcome_type == "categorical" && predictor_type == "categorical") {
    return("chi_square")
  }

  # If outcome categorical + predictor numeric → logistic regression
  if (outcome_type == "categorical" && predictor_type == "numeric") {
    return("logistic_regression")
  }

  # Otherwise unsupported
  warning("Unsupported combination of variable types.")
  return(NA_character_)
}
