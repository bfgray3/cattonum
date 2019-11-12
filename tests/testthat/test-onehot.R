y <- 2^seq(from = 0, to = 4)
x1 <- c("a", "b", NA, "b", "a")
x2 <- c("c", "c", "c", "d", "d")

df_fact <- data.frame(y, x1, x2)
df_char <- data.frame(y, x1, x2, stringsAsFactors = FALSE)

expected_x1_only <- data.frame(
  y,
  x2,
  x1a = c(1, 0, NA, 0, 1),
  x1b = c(0, 1, NA, 1, 0)
)

test_that("catto_onehot() correctly encodes train data.", {

  ### ALL CATEGORICAL COLUMNS ###

  oh_fact <- catto_onehot(df_fact)
  oh_char <- catto_onehot(df_char)

  expected_df_both <- data.frame(
    y = y,
    x1a = c(1, 0, NA, 0, 1),
    x1b = c(0, 1, NA, 1, 0),
    x2c = c(1, 1, 1, 0, 0),
    x2d = c(0, 0, 0, 1, 1)
  )

  expect_equal(oh_fact, cattonum_df(expected_df_both))
  expect_equal(oh_char, cattonum_df(expected_df_both))

  ### SUBSET OF CATEGORICAL COLUMNS ###

  expect_equal(catto_onehot(df_fact, "x1"), cattonum_df(expected_x1_only))
  expect_equal(catto_onehot(df_fact, x1), cattonum_df(expected_x1_only))
})

test_that("catto_onehot() correctly encodes test data.", {
  test_df <- data.frame(
    y = seq(3),
    x1 = c("e", NA, "b"),
    x2 = c("c", "d", "c")
  )

  expected_test <- data.frame(
    y = seq(3),
    x2 = c("c", "d", "c"),
    x1a = c(NA, NA, 0),
    x1b = c(NA, NA, 1)
  )

  expect_equal(
    catto_onehot(df_fact, x1, test = test_df),
    cattonum_df2(train = expected_x1_only, test = expected_test)
  )
})

test_that("catto_onehot() handles many columns.", {
  wide <- as.data.frame(matrix(c("a", "b"), nrow = 2, ncol = 5e3))
  expect_silent(encoded_wide <- catto_onehot(wide))
  expect_identical(c(nrow(wide), 2L * ncol(wide)), dim(encoded_wide))
})
