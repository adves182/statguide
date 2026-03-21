test_that("identify_var_type correctly identifies numeric and categorical", {
  result <- identify_var_type(PlantGrowth, "weight", "group")

  expect_equal(result$outcome_type, "numeric")
  expect_equal(result$predictor_type, "categorical")
})

test_that("identify_var_type handles character predictors", {
  df <- data.frame(
    y = 1:5,
    x = c("a", "b", "a", "b", "a")
  )

  result <- identify_var_type(df, "y", "x")

  expect_equal(result$outcome_type, "numeric")
  expect_equal(result$predictor_type, "categorical")
})

test_that("identify_var_type returns 'other' for unsupported types", {
  df <- data.frame(
    y = 1:5,
    x = I(list(1, 2, 3, 4, 5))  # list column
  )

  result <- identify_var_type(df, "y", "x")

  expect_equal(result$outcome_type, "numeric")
  expect_equal(result$predictor_type, "other")
})
