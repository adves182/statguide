test_that("explain_choice works for t-test", {
  df <- data.frame(
    y = rnorm(20),
    x = factor(rep(c("A", "B"), each = 10))
  )

  out <- explain_choice(df, "y", "x")

  expect_equal(out$test, "t_test")
  expect_true(grepl("two groups", out$explanation))
  expect_true(grepl("normal", out$explanation))
})

test_that("explain_choice works for ANOVA", {
  df <- data.frame(
    y = rnorm(30),
    x = factor(rep(c("A", "B", "C"), each = 10))
  )

  out <- explain_choice(df, "y", "x")

  expect_equal(out$test, "anova")
  expect_true(grepl("more than two groups", out$explanation))
  expect_true(grepl("variances", out$explanation))
})

test_that("explain_choice works for correlation", {
  df <- data.frame(
    y = rnorm(20),
    x = rnorm(20)
  )

  out <- explain_choice(df, "y", "x")

  expect_equal(out$test, "correlation")
  expect_true(grepl("association", out$explanation))
})

test_that("explain_choice works for chi-square", {
  df <- data.frame(
    y = factor(rep(c("yes", "no"), each = 20)),
    x = factor(rep(c("A", "B"), times = 20))
  )

  out <- explain_choice(df, "y", "x")

  expect_equal(out$test, "chi_square")
  expect_true(grepl("expected counts", out$explanation))
})

test_that("explain_choice works for logistic regression", {
  df <- data.frame(
    y = factor(rep(c("yes", "no"), each = 20)),
    x = rnorm(40)
  )

  out <- explain_choice(df, "y", "x")

  expect_equal(out$test, "logistic_regression")
  expect_true(grepl("probability", out$explanation))
})

test_that("explain_choice errors if choose_test returns NULL", {
  fake_data <- data.frame(a = 1:5, b = 1:5)

  # Temporarily override choose_test
  local_mocked_bindings(
    choose_test = function(...) NULL
  )

  expect_error(explain_choice(fake_data, "a", "b"))
})
