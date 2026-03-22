#' Explain the reasoning behind the chosen statistical test
#'
#' Provides a plain-language explanation of why a particular statistical test
#' was selected based on the variable types and structure of the data. This
#' function is designed to support learning by making the decision process
#' transparent and easy to follow.
#'
#' @param data A data frame containing the variables.
#' @param outcome A string giving the name of the outcome variable.
#' @param predictor A string giving the name of the predictor variable.
#'
#' @return A character string explaining the logic behind the test choice.
#'
#' @examples
#' # Explanation for ANOVA selection
#' explain_choice(PlantGrowth, "weight", "group")
#'
#' # Explanation for correlation
#' df <- data.frame(x = rnorm(20), y = rnorm(20))
#' explain_choice(df, "x", "y")
#'
#' @export
explain_choice <- function(data, outcome, predictor) {

  test <- choose_test(data, outcome, predictor)

  if (is.null(test)) {
    stop("choose_test() returned NULL, cannot generate explanation.")
  }

  y <- data[[outcome]]
  x <- data[[predictor]]

  y_type <- identify_var_type(y)
  x_type <- identify_var_type(x)

  n_groups <- if (x_type == "categorical") count_groups(x) else NA
  n_groups_text <- if (!is.na(n_groups)) paste0(" (", n_groups, ")") else ""

  explanation <- switch(
    test,

    "t_test" = paste0(
      "A t-test was selected because the outcome variable '", outcome,
      "' is numeric and the predictor '", predictor,
      "' is categorical with two groups", n_groups_text, ". ",
      "This test compares the mean outcome between two independent groups, ",
      "assuming approximately normal distributions within groups."
    ),

    "anova" = paste0(
      "ANOVA was selected because the outcome variable '", outcome,
      "' is numeric and the predictor '", predictor,
      "' is categorical with three or more groups", n_groups_text, ". ",
      "ANOVA tests whether there are differences in mean outcome across groups, ",
      "assuming approximately normal residuals and similar variances."
    ),

    "correlation" = paste0(
      "A correlation test was selected because both '", outcome,
      "' and '", predictor,
      "' are numeric. This test assesses the strength of an association ",
      "(typically linear for Pearson correlation)."
    ),

    "chi_square" = paste0(
      "A chi-square test was selected because both '", outcome,
      "' and '", predictor,
      "' are categorical. This test evaluates whether the variables are associated, ",
      "assuming expected counts are sufficiently large in each cell."
    ),

    "logistic_regression" = paste0(
      "Logistic regression was selected because the outcome variable '", outcome,
      "' is binary. This model estimates the probability of the outcome ",
      "based on the predictor variable."
    ),

    paste0("The test '", test, "' was selected, but no explanation is available.")
  )

  list(
    test = test,
    explanation = explanation
  )
}

