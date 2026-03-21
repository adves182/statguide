#' Run a statistical test based on variable types and group structure
#'
#' This function selects and runs an appropriate statistical test based on the
#' outcome and predictor variables. It uses \code{choose_test()} to determine
#' which test to run, and then performs the corresponding analysis.
#'
#' @param data A data frame containing the variables.
#' @param outcome A string giving the name of the outcome variable.
#' @param predictor A string giving the name of the predictor variable.
#'
#' @return A list containing:
#' \describe{
#'   \item{test}{The name of the test performed}
#'   \item{result}{The model or test output object}
#' }
#'
#' @examples
#' run_test(PlantGrowth, outcome = "weight", predictor = "group")
#'
#' @export
run_test <- function(data, outcome, predictor) {

  test_type <- choose_test(data, outcome, predictor)

  # Extract variables
  y <- data[[outcome]]
  x <- data[[predictor]]

  # Run the appropriate test
  if (test_type == "t_test") {
    result <- t.test(y ~ x)

  } else if (test_type == "anova") {
    model <- aov(y ~ x, data = data)
    result <- summary(model)

  } else if (test_type == "correlation") {
    result <- cor.test(y, x)

  } else if (test_type == "chi_square") {
    tbl <- table(y, x)
    result <- chisq.test(tbl)

  } else if (test_type == "logistic_regression") {
    model <- glm(y ~ x, data = data, family = binomial)
    result <- summary(model)

  } else {
    stop("Unsupported test type or insufficient information to run a test.")
  }

  # Return a clean result
  list(
    test = test_type,
    result = result
  )
}
