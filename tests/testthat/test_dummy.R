context("dummy encoding")

#############
### SETUP ###
#############

y <- 2 ^ seq(from = 0, to = 4)
x1 <- c("a", "b", NA, "b", "a")
x2 <- c("c", "c", "c", "d", "d")

df_fact <- data.frame(y, x1, x2)
df_char <- data.frame(y, x1, x2, stringsAsFactors = FALSE)

expected_x1_only <- data.frame(y,
                               x2,
                               x1b = c(0, 1, NA, 1, 0))

##################
### TRAIN DATA ###
##################

test_that("catto_dummy correctly encodes train data.", {

  ### ALL CATEGORICAL COLUMNS ###

  dummy_fact <- catto_dummy(df_fact)
  dummy_char <- catto_dummy(df_char)

  expected_df_both <- data.frame(y = y,
                                 x1b = c(0, 1, NA, 1, 0),
                                 x2d = c(0, 0, 0, 1, 1))
  expected_both <- as.matrix(expected_df_both)

  expect_equal(dummy_fact, expected_both)
  expect_equal(dummy_char, expected_both)

  ### SUBSET OF CATEGORICAL COLUMNS ###

  expect_equal(catto_dummy(df_fact, "x1"), expected_x1_only)
  expect_equal(catto_dummy(df_fact, x1), expected_x1_only)

})

##################
### TEST DATA ###
##################

test_that("catto_dummy correctly encodes test data.", {

  test_df <- data.frame(y = seq_len(3),
                        x1 = c("e", NA, "b"),
                        x2 = c("c", "d","c"))

  expected_test <- data.frame(y = seq_len(3),
                              x2 = c("c", "d", "c"),
                              x1b = c(NA, NA, 1))

  expect_equal(catto_dummy(df_fact, x1, test = test_df),
               list(train = expected_x1_only, test = expected_test))

})

###