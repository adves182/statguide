#' Plot diagnostic visuals for the chosen statistical test
#'
#' Generates diagnostic plots appropriate for the statistical test selected by
#' \code{choose_test()}. These visuals help assess assumptions such as
#' normality, outliers, linearity, and model fit.
#'
#' @param data A data frame containing the variables.
#' @param outcome A string giving the name of the outcome variable.
#' @param predictor A string giving the name of the predictor variable.
#'
#' @return A named list containing ggplot objects (and observed/expected tables. Use `names(plots)` to see available diagnostics.
#'   for chi-square tests). Print individual plots using \code{plots$boxplot},
#'   \code{plots$qq}, or other elements returned in the list.
#'
#' @examples
#' # Diagnostics for numeric outcome vs categorical predictor
#' plots <- plot_diagnostics(PlantGrowth, "weight", "group")
#' plots$boxplot   # Boxplot of groups
#' plots$qq        # Q-Q plot for normality
#'
#' @import ggplot2
#' @importFrom stats aov chisq.test glm predict binomial cooks.distance
#' @export

plot_diagnostics <- function(data, outcome, predictor) {

  test_type <- choose_test(data, outcome, predictor)
  y <- data[[outcome]]
  x <- data[[predictor]]

  plots <- list()

  # -------------------------
  # t-test diagnostics
  # -------------------------
  if (test_type == "t_test") {
    plots$boxplot <- ggplot(data, aes(x = x, y = y)) +
      geom_boxplot(fill = "steelblue") +
      labs(title = "Boxplot of Outcome by Group")

    plots$qq <- ggplot(data, aes(sample = y)) +
      stat_qq() + stat_qq_line() +
      facet_wrap(~ x) +
      labs(title = "Q-Q Plot by Group")
  }

  # -------------------------
  # ANOVA diagnostics
  # -------------------------
  if (test_type == "anova") {
    model <- aov(y ~ x, data = data)
    residuals <- residuals(model)
    fitted <- fitted(model)

    plots$boxplot <- ggplot(data, aes(x = x, y = y)) +
      geom_boxplot(fill = "steelblue") +
      labs(title = "Boxplot of Outcome by Group")

    plots$residuals_fitted <- ggplot(data.frame(fitted, residuals),
                                     aes(x = fitted, y = residuals)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = "Residuals vs Fitted")

    plots$qq <- ggplot(data.frame(residuals),
                       aes(sample = residuals)) +
      stat_qq() + stat_qq_line() +
      labs(title = "Q-Q Plot of Residuals")
  }

  # -------------------------
  # Correlation diagnostics
  # -------------------------
  if (test_type == "correlation") {
    plots$scatter <- ggplot(data, aes(x = x, y = y)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Scatterplot with Regression Line")
  }

  # -------------------------
  # Chi-square diagnostics (ggplot mosaic)
  # -------------------------
  if (test_type == "chi_square") {
    tbl <- table(y, x)
    chi <- suppressWarnings(chisq.test(tbl))

    df_mosaic <- as.data.frame(tbl)
    colnames(df_mosaic) <- c("y", "x", "count")

    df_mosaic <- df_mosaic |>
      dplyr::group_by(x) |>
      dplyr::mutate(prop = count / sum(count))

    plots$mosaic <- ggplot(df_mosaic, aes(x = x, y = prop, fill = y)) +
      geom_col(position = "fill") +
      labs(title = "Mosaic Plot", y = "Proportion")

    plots$expected_observed <- list(
      observed = tbl,
      expected = chi$expected
    )
  }

  # -------------------------
  # Logistic regression diagnostics
  # -------------------------
  if (test_type == "logistic_regression") {
    y_factor <- as.factor(y)
    if (nlevels(y_factor) != 2) {
      stop("Logistic regression requires a binary outcome.")
    }

    model <- glm(y_factor ~ x, data = data, family = binomial)

    plots$residuals_fitted <- ggplot(data.frame(
      fitted = fitted(model),
      residuals = residuals(model, type = "deviance")
    ), aes(x = fitted, y = residuals)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = "Residuals vs Fitted")

    cooks <- cooks.distance(model)
    plots$cooks <- ggplot(data.frame(index = seq_along(cooks), cooks = cooks),
                          aes(x = index, y = cooks)) +
      geom_bar(stat = "identity") +
      labs(title = "Cook's Distance (Influence)")

    probs <- predict(model, type = "response")
    truth <- as.numeric(y_factor) - 1
    roc_obj <- pROC::roc(truth, probs)

    roc_df <- data.frame(
      tpr = roc_obj$sensitivities,
      fpr = 1 - roc_obj$specificities
    )

    plots$roc <- ggplot(roc_df, aes(x = fpr, y = tpr)) +
      geom_line() +
      labs(title = "ROC Curve", x = "False Positive Rate", y = "True Positive Rate")
  }

  return(plots)
}
