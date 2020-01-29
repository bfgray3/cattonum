context("conditions")

# TODO: test dplyr/tidyselect syntax better here and everywhere

test_that("conditions work correctly.", {
  foo <- data.frame(a = 1, b = "e")
  bar <- data.frame(c = 2, d = "f")

  expect_error(
    catto_label(foo, all_of(x1)),
    regexp = "'all_of(x1)' is not a valid column specification for foo.",
    fixed = TRUE
  )

  expect_error(
    check_train_test(foo, bar),
    regexp = "`train` and `test` data frames must have the same names.",
    fixed = TRUE
  )

  expect_message(
    catto_mean(foo),
    regexp = "`response` not supplied; using first column 'a' as the response variable.",
    fixed = TRUE
  )
})
