test_that("run_test performs ANOVA for PlantGrowth", {
  out <- run_test(PlantGrowth, "weight", "group")
  expect_equal(out$test, "anova")
  expect_true(inherits(out$result, "summary.aov"))
})

test_that("run_test performs t-test for 2 groups", {
  df <- data.frame(
    y = rnorm(10),
    x = factor(rep(c("A", "B"), each = 5))
  )
  out <- run_test(df, "y", "x")
  expect_equal(out$test, "t_test")
  expect_true(inherits(out$result, "htest"))
})

test_that("run_test performs correlation for numeric-numeric", {
  df <- data.frame(
    y = rnorm(10),
    x = rnorm(10)
  )
  out <- run_test(df, "y", "x")
  expect_equal(out$test, "correlation")
  expect_true(inherits(out$result, "htest"))
})

test_that("run_test performs chi-square for categorical-categorical", {
  df <- data.frame(
    y = factor(rep(c("yes", "no"), each = 20)),
    x = factor(rep(c("A", "B"), times = 20))
  )

  out <- run_test(df, "y", "x")

  expect_equal(out$test, "chi_square")
  expect_true(inherits(out$result, "htest"))
})

