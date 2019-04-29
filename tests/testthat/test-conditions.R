context("conditions")

test_that("conditions work correctly.", {
  expect_error(catto_freq(data.frame(x = c(TRUE, FALSE))),
    regexp = "All columns must be numeric, character, or factor. 'x' is not.",
    fixed = TRUE
  )

  expect_error(catto_freq(data.frame(x = c(TRUE, FALSE), y = c(TRUE, FALSE))),
    regexp = "All columns must be numeric, character, or factor. 'x', 'y' are not.",
    fixed = TRUE
  )

  foo <- data.frame(a = 1, b = "e")
  bar <- data.frame(c = 2, d = "f")

  expect_error(catto_label(foo, one_of(x1, x2)),
    regexp = "'one_of(x1, x2)' is not a valid column specification for foo.",
    fixed = TRUE
  )

  expect_error(check_train_test(foo, bar),
    regexp = "`train` and `test` data frames must have the same names.",
    fixed = TRUE
  )

  expect_message(catto_mean(foo),
    regexp = "`response` not supplied; using first column 'a' as the response variable.",
    fixed = TRUE
  )
})
