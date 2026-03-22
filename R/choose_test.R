#' Choose an appropriate statistical test
#'
#' Determines the appropriate statistical test based on the variable types of
#' the outcome and predictor. This function identifies whether variables are
#' numeric or categorical and selects a suitable introductory-level test such as
#' a t-test, ANOVA, correlation, chi-square test, or logistic regression.
#'
#' @param data A data frame containing the variables.
#' @param outcome A string giving the name of the outcome variable.
#' @param predictor A string giving the name of the predictor variable.
#'
#' @return A character string naming the selected statistical test.
#'
#' @examples
#' # Numeric outcome and categorical predictor → ANOVA
#' choose_test(PlantGrowth, "weight", "group")
#'
#' # Numeric–numeric relationship → correlation
#' df <- data.frame(x = rnorm(20), y = rnorm(20))
#' choose_test(df, "x", "y")
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
