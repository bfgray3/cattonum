expected_df_both <- data.frame(
  y = y,
  x1 = c(3L, 2L, NA_integer_, 2L, 3L, 3L),
  x2 = as.integer(c(4, 4, 4, 2, 2, 4))
)

expected_tbl_both <- as_tibble(expected_df_both)

expected_x1_df_fact <- data.frame(y, x1 = c(3L, 2L, NA_integer_, 2L, 3L, 3L), x2)

expected_x1_df_char <- data.frame(
  y,
  x1 = as.integer(c(3, 2, NA, 2, 3, 3)),
  x2,
  stringsAsFactors = FALSE
)

expected_x1_tbl_char <- tibble(y, x1 = as.integer(c(3, 2, NA, 2, 3, 3)), x2)

expected_x1_tbl_fact <- tibble(
  y,
  x1 = as.integer(c(3, 2, NA, 2, 3, 3)),
  x2 = factor(x2)
)

test_df <- data.frame(
  y = y[seq(5)],
  x1 = c(NA, NA, "a", "b", "b"),
  x2 = c("d", NA, NA, "c", "c")
)

encoded_test <- data.frame(
  y = y[seq(5)],
  x1 = c(NA, NA, 3, 2, 2),
  x2 = c(2, NA, NA, 4, 4)
)

test_that("catto_freq(): multiple data.frame training columns.", {
  both_encoded <- check_x1_x2(catto_freq, "data.frame")
  for (m in both_encoded) expect_equal(m, cattonum_df(expected_df_both))
})

test_that("catto_freq(): multiple tibble training columns.", {
  both_encoded <- check_x1_x2(catto_freq, "tibble")
  for (m in both_encoded) expect_equal(m, cattonum_df(expected_tbl_both))
})

test_that("catto_freq(): one data.frame training column.", {
  one_encoded <- check_x1(catto_freq, "data.frame")
  num_tests <- length(one_encoded)

  for (i in seq(from = 1, to = num_tests / 2)) {
    expect_equal(one_encoded[[i]], cattonum_df(expected_x1_df_fact))
  }

  for (i in seq(from = num_tests / 2 + 1, to = num_tests)) {
    expect_equal(one_encoded[[i]], cattonum_df(expected_x1_df_char))
  }
})

test_that("catto_freq: one tibble training column.", {
  one_encoded <- check_x1(catto_freq, "tibble")
  num_tests <- length(one_encoded)

  for (i in seq(from = 1, to = num_tests / 2)) {
    expect_equal(one_encoded[[i]], cattonum_df(expected_x1_tbl_fact))
  }

  for (i in seq(from = num_tests / 2 + 1, to = num_tests)) {
    expect_equal(one_encoded[[i]], cattonum_df(expected_x1_tbl_char))
  }
})

test_that("catto_freq() correctly encodes test data.", {
  expect_equal(
    catto_freq(df_fact, test = test_df),
    cattonum_df2(train = expected_df_both, test = encoded_test)
  )
})
