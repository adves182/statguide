test_that("plot_diagnostics works for t-test", {
  df <- data.frame(
    y = rnorm(20),
    x = factor(rep(c("A", "B"), each = 10))
  )

  plots <- plot_diagnostics(df, "y", "x")

  expect_true("boxplot" %in% names(plots))
  expect_true("qq" %in% names(plots))
  expect_s3_class(plots$boxplot, "ggplot")
})

test_that("plot_diagnostics works for ANOVA", {
  df <- data.frame(
    y = rnorm(30),
    x = factor(rep(c("A", "B", "C"), each = 10))
  )

  plots <- plot_diagnostics(df, "y", "x")

  expect_true("boxplot" %in% names(plots))
  expect_true("residuals_fitted" %in% names(plots))
  expect_true("qq" %in% names(plots))
  expect_s3_class(plots$boxplot, "ggplot")
})

test_that("plot_diagnostics works for correlation", {
  df <- data.frame(
    y = rnorm(20),
    x = rnorm(20)
  )

  plots <- plot_diagnostics(df, "y", "x")

  expect_true("scatter" %in% names(plots))
  expect_s3_class(plots$scatter, "ggplot")
})

test_that("plot_diagnostics works for chi-square", {
  df <- data.frame(
    y = factor(rep(c("yes", "no"), each = 20)),
    x = factor(rep(c("A", "B"), times = 20))
  )

  plots <- suppressWarnings(plot_diagnostics(df, "y", "x"))

  expect_true("mosaic" %in% names(plots))
  expect_s3_class(plots$mosaic, "ggplot")
  expect_true("expected_observed" %in% names(plots))
})

test_that("plot_diagnostics works for logistic regression", {
  df <- data.frame(
    y = factor(rep(c("yes", "no"), each = 20)),
    x = rnorm(40)
  )

  plots <- plot_diagnostics(df, "y", "x")

  expect_true("residuals_fitted" %in% names(plots))
  expect_true("cooks" %in% names(plots))
  expect_true("roc" %in% names(plots))
  expect_s3_class(plots$roc, "ggplot")
})

test_that("logistic regression requires binary outcome", {
  df <- data.frame(
    y = factor(c("A", "B", "C")),
    x = rnorm(3)
  )

  expect_error(plot_diagnostics(df, "y", "x"), "binary outcome")
})
