expected_df_both <- data.frame(
  y = y,
  x1 = c(16, 5, NA, 5, 16, 16),
  x2 = c(3, 3, 3, 12, 12, 3)
)

expected_tbl_both <- as_tibble(expected_df_both)

expected_x1_df_fact <- data.frame(
  y,
  x1 = c(16, 5, NA, 5, 16, 16),
  x2
)

expected_x1_df_char <- data.frame(
  y,
  x1 = c(16, 5, NA, 5, 16, 16),
  x2,
  stringsAsFactors = FALSE
)

expected_x1_tbl_char <- tibble(y, x1 = c(16, 5, NA, 5, 16, 16), x2)

expected_x1_tbl_fact <- tibble(y, x1 = c(16, 5, NA, 5, 16, 16), x2 = factor(x2))

encoded_test <- data.frame(
  y = y[seq(5)],
  x1 = c(NA, NA, 16, 5, 5),
  x2 = c(12, NA, NA, 3, 3)
)

test_that("catto_median(): multiple data.frame training columns.", {
  both_encoded <- check_x1_x2_resp(catto_median, "data.frame", .resp = resp_name)
  for (m in both_encoded) expect_equal(m, cattonum_df(expected_df_both))
})

test_that("catto_median(): multiple tibble training columns.", {
  both_encoded <- check_x1_x2_resp(catto_median, "tibble", .resp = resp_name)
  for (m in both_encoded) expect_equal(m, cattonum_df(expected_tbl_both))
})

test_that("catto_median(): one data.frame training column.", {
  one_encoded <- check_x1_resp(catto_median, "data.frame", .resp = resp_name)
  num_tests <- length(one_encoded)

  for (i in seq(from = 1, to = num_tests / 2)) {
    expect_equal(one_encoded[[i]], cattonum_df(expected_x1_df_fact))
  }

  for (i in seq(from = num_tests / 2 + 1, to = num_tests)) {
    expect_equal(one_encoded[[i]], cattonum_df(expected_x1_df_char))
  }
})

test_that("catto_median(): one tibble training column.", {
  one_encoded <- check_x1_resp(catto_median, "tibble", .resp = resp_name)
  num_tests <- length(one_encoded)

  for (i in seq(from = 1, to = num_tests / 2)) {
    expect_equivalent(one_encoded[[i]], expected_x1_tbl_fact)
  }

  for (i in seq(from = num_tests / 2 + 1, to = num_tests)) {
    expect_equivalent(one_encoded[[i]], expected_x1_tbl_char)
  }
})

test_that("catto_median() correctly encodes test data.", {
  expect_equal(
    catto_median(df_fact, test = test_df),
    cattonum_df2(train = expected_df_both, test = encoded_test)
  )
})
