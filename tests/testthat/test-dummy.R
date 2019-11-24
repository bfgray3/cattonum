expected_df_both <- data.frame(
  y = y,
  x1b = c(0, 1, NA, 1, 0, 0),
  x2d = c(0, 0, 0, 1, 1, 0)
)

expected_tbl_both <- as_tibble(expected_df_both)

expected_x1_df_fact <- data.frame(y, x2, x1b = c(0, 1, NA, 1, 0, 0))

expected_x1_df_char <- data.frame(
  y,
  x2,
  x1b = c(0, 1, NA, 1, 0, 0),
  stringsAsFactors = FALSE
)

expected_x1_tbl_char <- tibble(y, x2, x1b = c(0, 1, NA, 1, 0, 0))

expected_x1_tbl_fact <- tibble(y, x2 = factor(x2), x1b = c(0, 1, NA, 1, 0, 0))

test_that("catto_dummy(): multiple data.frame training columns.", {
  both_encoded <- check_x1_x2(catto_dummy, "data.frame")
  for (m in both_encoded) expect_equal(m, cattonum_df(expected_df_both))
})

test_that("catto_dummy(): multiple tibble training columns.", {
  both_encoded <- check_x1_x2(catto_dummy, "tibble")
  for (m in both_encoded) expect_equal(m, cattonum_df(expected_tbl_both))
})

test_that("catto_dummy(): one data.frame training column.", {
  one_encoded <- check_x1(catto_dummy, "data.frame")
  num_tests <- length(one_encoded)

  for (i in seq(from = 1, to = num_tests / 2)) {
    expect_equal(one_encoded[[i]], cattonum_df(expected_x1_df_fact))
  }

  for (i in seq(from = num_tests / 2 + 1, to = num_tests)) {
    expect_equal(one_encoded[[i]], cattonum_df(expected_x1_df_char))
  }
})

test_that("catto_dummy(): one tibble training column.", {
  one_encoded <- check_x1(catto_dummy, "tibble")
  num_tests <- length(one_encoded)

  for (i in seq(from = 1, to = num_tests / 2)) {
    expect_equal(one_encoded[[i]], cattonum_df(expected_x1_tbl_fact))
  }

  for (i in seq(from = num_tests / 2 + 1, to = num_tests)) {
    expect_equal(one_encoded[[i]], cattonum_df(expected_x1_tbl_char))
  }
})

test_that("catto_dummy() correctly encodes test data.", {
  small_test_df <- data.frame(
    y = seq(3),
    x1 = c("e", NA, "b"),
    x2 = c("c", "d", "c")
  )

  expected_test <- data.frame(
    y = seq(3),
    x2 = c("c", "d", "c"),
    x1b = c(NA, NA, 1)
  )

  expect_equal(
    catto_dummy(df_fact, x1, test = small_test_df),
    cattonum_df2(train = expected_x1_df_fact, test = expected_test)
  )
})

test_that("catto_dummy() handles many columns.", {
  wide <- as.data.frame(matrix(c("a", "b"), nrow = 2, ncol = 5e3))
  expect_silent(encoded_wide <- catto_dummy(wide))
  expect_identical(dim(wide), dim(encoded_wide))
})

test_that("catto_dummy: logicals in data.frame training columns.", {
  df_logi_expected <- data.frame(
    y = y,
    x1TRUE = c(1, 1, 0, 0, 0, 1),
    x2TRUE = c(1, 1, NA, 0, 0, 1)
  )
  expect_equal(
    catto_dummy(df_logi),
    cattonum_df(df_logi_expected)
  )
})

test_that("catto_dummy: logicals in tibble training columns.", {
  tbl_logi_expected <- tibble(
    y = y,
    x1TRUE = c(1, 1, 0, 0, 0, 1),
    x2TRUE = c(1, 1, NA, 0, 0, 1)
  )
  expect_equal(
    catto_dummy(tbl_logi),
    cattonum_df(tbl_logi_expected)
  )
})
