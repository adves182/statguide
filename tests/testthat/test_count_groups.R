test_that("count_groups works for factor predictors", {
  df <- data.frame(x = factor(c("A", "B", "A")))
  expect_equal(count_groups(df$x), 2)
})

test_that("count_groups works for character predictors", {
  df <- data.frame(x = c("yes", "no", "yes"))
  expect_equal(count_groups(df$x), 2)
})

test_that("count_groups errors for numeric predictors", {
  df <- data.frame(x = c(1, 2, 3))
  expect_error(count_groups(df$x))
})

test_that("count_groups errors for unsupported types", {
  df <- data.frame(x = list(1, 2, 3))
  expect_error(count_groups(df$x))
})
