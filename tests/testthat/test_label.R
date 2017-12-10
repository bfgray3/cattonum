context("label encoding")

#############
### SETUP ###
#############

y <- 2 ^ seq(from = 0, to = 5)
x1 <- c("a", "b", NA, "b", "a", "a")
x2 <- c("c", "c", "c", "c", "d", "d")

df_fact <- data.frame(y = y, x1 = x1, x2 = x2)
df_char <- data.frame(y = y, x1 = x1, x2 = x2, stringsAsFactors = FALSE)

######################
### LABEL ENCODING ###
######################

test_that("catto_label works.", {

  ### ALL CATEGORICAL COLUMNS ###

  label_fact1 <- catto_label(df_fact)
  label_fact2 <- catto_label(df_fact, x1, x2)
  label_fact3 <- catto_label(df_fact, c(x1, x2))
  label_fact4 <- catto_label(df_fact, c("x1", "x2"))
  label_fact5 <- catto_label(df_fact, tidyselect::one_of(c("x1", "x2")))
  label_fact6 <- catto_label(df_fact, tidyselect::one_of("x1", "x2"))
  label_char1 <- catto_label(df_char)
  label_char2 <- catto_label(df_char, x1, x2)
  label_char3 <- catto_label(df_char, c(x1, x2))
  label_char4 <- catto_label(df_char, c("x1", "x2"))
  label_char5 <- catto_label(df_char, tidyselect::one_of(c("x1", "x2")))
  label_char6 <- catto_label(df_char, tidyselect::one_of("x1", "x2"))

  expected_df_both <- data.frame(y = y,
                                 x1 = c(2, 1, NA, 1, 2, 2),
                                 x2 = c(2, 2, 2, 2, 1, 1))
  expected_both <- as.matrix(expected_df_both)
  result_names <- c(paste0("label_fact", seq_len(6)), paste0("label_char", seq_len(6)))
  char_and_fact <- mget(result_names)

  for (m in char_and_fact) {

    expect_is(m, "matrix")
    expect_equal(m, expected_both)

  }

  ### SUBSET OF CATEGORICAL COLUMNS ###

  expected_x1_only <- data.frame(y = y, x1 = c(2, 1, NA, 1, 2, 2), x2 = x2)
  char_and_bare <- list(catto_label(df_fact, "x1"),
                        catto_label(df_fact, x1),
                        catto_label(df_fact, tidyselect::one_of("x1")))

  for (result in char_and_bare) {

    expect_is(result, "data.frame")
    expect_equal(result, expected_x1_only)

  }

})

###