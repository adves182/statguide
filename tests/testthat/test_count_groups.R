test_that("count_groups works for factor predictors", {
  result <- count_groups(PlantGrowth, "group")
  expect_equal(result, 3)
})

test_that("count_groups works for character predictors", {
  df <- data.frame(
    y = 1:5,
    x = c("a", "b", "a", "b", "c")
  )
  result <- count_groups(df, "x")
  expect_equal(result, 3)
})

test_that("count_groups works for numeric predictors", {
  df <- data.frame(
    y = 1:5,
    x = c(1, 1, 2, 3, 3)
  )
  result <- count_groups(df, "x")
  expect_equal(result, 3)
})

test_that("count_groups returns NA for unsupported types", {
  df <- data.frame(
    y = 1:5,
    x = I(list(1, 2, 3, 4, 5))
  )
  result <- suppressWarnings(count_groups(df, "x"))
  expect_true(is.na(result))
})
