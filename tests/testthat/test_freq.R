context("frequency encoding")

#############
### SETUP ###
#############

y <- 2 ^ seq(from = 0, to = 4)
x1 <- c("a", "b", NA, "b", "a")
x2 <- c("c", "c", "c", "d", "d")

df_fact <- data.frame(y, x1, x2)
df_char <- data.frame(y, x1, x2, stringsAsFactors = FALSE)

##########################
### FREQUENCY ENCODING ###
##########################

test_that("catto_freq works.", {

  ### ALL CATEGORICAL COLUMNS ###

  freq_fact1 <- catto_freq(df_fact)
  freq_fact2 <- catto_freq(df_fact, x1, x2)
  freq_fact3 <- catto_freq(df_fact, c(x1, x2))
  freq_fact4 <- catto_freq(df_fact, c("x1", "x2"))
  freq_fact5 <- catto_freq(df_fact, tidyselect::one_of(c("x1", "x2")))
  freq_fact6 <- catto_freq(df_fact, tidyselect::one_of("x1", "x2"))
  freq_char1 <- catto_freq(df_char)
  freq_char2 <- catto_freq(df_char, x1, x2)
  freq_char3 <- catto_freq(df_char, c(x1, x2))
  freq_char4 <- catto_freq(df_char, c("x1", "x2"))
  freq_char5 <- catto_freq(df_fact, tidyselect::one_of(c("x1", "x2")))
  freq_char6 <- catto_freq(df_fact, tidyselect::one_of("x1", "x2"))

  expected_df_both <- data.frame(y = y,
                                 x1 = c(2, 2, NA, 2, 2),
                                 x2 = c(3, 3, 3, 2, 2))
  expected_both <- as.matrix(expected_df_both)
  result_names <- c(paste0("freq_fact", seq_len(6)), paste0("freq_char", seq_len(6)))
  char_and_fact <- mget(result_names)

  for (m in char_and_fact) {

    expect_is(m, "matrix")
    expect_equal(m, expected_both)

  }

  ### SUBSET OF CATEGORICAL COLUMNS ###

  expected_x1_only <- data.frame(y, x1 = c(2, 2, NA, 2, 2), x2)
  char_and_bare <- list(catto_freq(df_fact, "x1", response = "y"),
                        catto_freq(df_fact, x1, response = y),
                        catto_freq(df_fact,
                                   tidyselect::one_of("x1"),
                                   response = "y"))

  for (result in char_and_bare) {

    expect_is(result, "data.frame")
    expect_equal(result, expected_x1_only)

  }

  ### RESPONSE NOT SPECIFIED ###

  expect_message(no_response <- catto_freq(df_fact, x1),
                 paste("`response` not supplied; using first column",
                       "'y' as the response variable."))
  expect_equal(no_response, expected_x1_only)

})

###