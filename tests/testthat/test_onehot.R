context("one-hot encoding")

#############
### SETUP ###
#############

y <- 2 ^ seq(from = 0, to = 4)
x1 <- c("a", "b", NA, "b", "a")
x2 <- c("c", "c", "c", "d", "d")

df_fact <- data.frame(y, x1, x2)
df_char <- data.frame(y, x1, x2, stringsAsFactors = FALSE)

########################
### ONE-HOT ENCODING ###
########################

test_that("catto_onehot works.", {

  ### ALL CATEGORICAL COLUMNS ###

  mean_fact <- catto_onehot(df_fact)
  mean_char <- catto_onehot(df_char)

  expected_df_both <- data.frame(y = y,
                                 x1a = c(1, 0, 0, 0, 1),
                                 x1b = c(0, 1, 0, 1, 0),
                                 x1NA = c(0, 0, 1, 0, 0),
                                 x2c = c(1, 1, 1, 0, 0),
                                 x2d = c(0, 0, 0, 1, 1))
  expected_both <- as.matrix(expected_df_both)
  char_and_fact <- list(mean_fact, mean_char)

  for (m in char_and_fact) {

    expect_is(m, "matrix")
    expect_equal(m, expected_both)

  }

  ### SUBSET OF CATEGORICAL COLUMNS ###

  expected_x1_only <- data.frame(y,
                                 x2,
                                 x1a = c(1, 0, 0, 0, 1),
                                 x1b = c(0, 1, 0, 1, 0),
                                 x1NA = c(0, 0, 1, 0, 0))
  char_and_bare <- list(catto_onehot(df_fact, "x1"),
                        catto_onehot(df_fact, x1))

  for (result in char_and_bare) {

    expect_is(result, "data.frame")
    expect_equal(result, expected_x1_only)

  }

})

###