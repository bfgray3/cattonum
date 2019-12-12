# FIXME: using expect_identical() not expect_equal() here
# cause of the bug described in issue #23

expected_df_both <- data.frame(
  y = y,
  x1 = c(24, 8, NA, 2, 16.5, 8.5),
  x2 = c(38 / 3, 37 / 3, 35 / 3, 16, 8, 7 / 3)
)

expected_tbl_both <- as_tibble(expected_df_both)

expected_x1_df_fact <- data.frame(y, x1 = c(24, 8, NA, 2, 16.5, 8.5), x2)

expected_x1_df_char <- data.frame(y,
  x1 = c(24, 8, NA, 2, 16.5, 8.5),
  x2,
  stringsAsFactors = FALSE
)

expected_x1_tbl_char <- tibble(y, x1 = c(24, 8, NA, 2, 16.5, 8.5), x2)

expected_x1_tbl_fact <- tibble(
  y,
  x1 = c(24, 8, NA, 2, 16.5, 8.5),
  x2 = factor(x2)
)

encoded_test <- data.frame(
  y = y[seq(5)],
  x1 = c(NA, NA, 49 / 3, 5, 5),
  x2 = c(12, NA, NA, 39 / 4, 39 / 4)
)

test_that("catto_loo(): multiple data.frame training columns.", {
  both_encoded <- check_x1_x2_resp(catto_loo, "data.frame", .resp = resp_name)
  for (m in both_encoded) expect_identical(m, cattonum_df(expected_df_both))
})

test_that("catto_loo(): multiple tibble training columns.", {
  both_encoded <- check_x1_x2_resp(catto_loo, "tibble", .resp = resp_name)
  for (m in both_encoded) expect_identical(m, cattonum_df(expected_tbl_both))
})

test_that("catto_loo: one data.frame training column.", {
  one_encoded <- check_x1_resp(catto_loo, "data.frame", .resp = resp_name)
  num_tests <- length(one_encoded)

  for (i in seq(from = 1, to = num_tests / 2)) {
    expect_identical(one_encoded[[i]], cattonum_df(expected_x1_df_fact))
  }

  for (i in seq(from = num_tests / 2 + 1, to = num_tests)) {
    expect_identical(one_encoded[[i]], cattonum_df(expected_x1_df_char))
  }
})

test_that("catto_loo: one tibble training column.", {
  one_encoded <- check_x1_resp(catto_loo, "tibble", .resp = resp_name)
  num_tests <- length(one_encoded)

  for (i in seq(from = 1, to = num_tests / 2)) {
    expect_identical(one_encoded[[i]], cattonum_df(expected_x1_tbl_fact))
  }

  for (i in seq(from = num_tests / 2 + 1, to = num_tests)) {
    expect_identical(one_encoded[[i]], cattonum_df(expected_x1_tbl_char))
  }
})

test_that("catto_loo correctly encodes test data.", {
  expect_identical(
    catto_loo(df_fact, test = test_df),
    cattonum_df2(train = expected_df_both, test = encoded_test)
  )
})
