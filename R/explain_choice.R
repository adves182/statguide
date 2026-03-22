#' Explain why a statistical test was chosen
#'
#' Provides a clear, structured explanation of why \code{choose_test()}
#' selected a particular statistical test. The explanation is based on
#' variable types, number of groups, and the relationship between outcome
#' and predictor variables.
#'
#' @param data A data frame containing the variables.
#' @param outcome A string giving the name of the outcome variable.
#' @param predictor A string giving the name of the predictor variable.
#'
#' @return A list with two elements:
#'   \itemize{
#'     \item \code{test}: the name of the chosen test
#'     \item \code{explanation}: a human-readable explanation
#'   }
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

