test_that("identify_var_type correctly identifies numeric and categorical", {
  expect_equal(identify_var_type(1:10), "numeric")
  expect_equal(identify_var_type(factor(c("A", "B"))), "categorical")
})

test_that("identify_var_type handles character vectors", {
  expect_equal(identify_var_type(c("yes", "no")), "categorical")
})

test_that("identify_var_type errors for unsupported types", {
  expect_error(identify_var_type(list(1, 2, 3)))
})
