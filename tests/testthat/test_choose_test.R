test_that("choose_test selects t-test for 2 groups", {
  result <- choose_test(PlantGrowth, "weight", "group")
  expect_equal(result, "anova")  # PlantGrowth has 3 groups
})

test_that("choose_test selects t-test for 2-group numeric outcome", {
  df <- data.frame(
    y = rnorm(10),
    x = factor(rep(c("A", "B"), each = 5))
  )
  result <- choose_test(df, "y", "x")
  expect_equal(result, "t_test")
})

test_that("choose_test selects correlation for numeric-numeric", {
  df <- data.frame(
    y = rnorm(10),
    x = rnorm(10)
  )
  result <- choose_test(df, "y", "x")
  expect_equal(result, "correlation")
})

test_that("choose_test selects chi-square for categorical-categorical", {
  df <- data.frame(
    y = factor(rep(c("yes", "no"), each = 5)),
    x = factor(rep(c("A", "B"), times = 5))
  )
  result <- choose_test(df, "y", "x")
  expect_equal(result, "chi_square")
})

test_that("choose_test selects logistic regression for categorical outcome + numeric predictor", {
  df <- data.frame(
    y = factor(rep(c("yes", "no"), each = 5)),
    x = rnorm(10)
  )
  result <- choose_test(df, "y", "x")
  expect_equal(result, "logistic_regression")
})
