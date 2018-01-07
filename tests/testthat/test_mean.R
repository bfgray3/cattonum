context("mean encoding")

#############
### SETUP ###
#############

resp_name <- "y"
y <- 2 ^ seq(from = 0, to = 4)
x1 <- c("a", "b", NA, "b", "a")
x2 <- c("c", "c", "c", "d", "d")

df_fact <- data.frame(y, x1, x2)
df_char <- data.frame(y, x1, x2, stringsAsFactors = FALSE)

test_df <- data.frame(y = y,
                      x1 = c(NA, NA, "a", "b", "b"),
                      x2 = c("d", NA, NA, "c", "c"))

encoded_test <- data.frame(y = y,
                           x1 = c(NA, NA, 8.5, 5, 5),
                           x2 = c(12, NA, NA, 7 / 3, 7 / 3))

#####################
### MEAN ENCODING ###
#####################

test_that("catto_mean correctly encodes train data.", {

  ### ALL CATEGORICAL COLUMNS ###

  mean_fact1 <- catto_mean(df_fact, response = "y")
  mean_fact2 <- catto_mean(df_fact, x1, x2, response = y)
  mean_fact3 <- catto_mean(df_fact, c(x1, x2), response = y)
  mean_fact4 <- catto_mean(df_fact, c("x1", "x2"), response = resp_name)
  mean_fact5 <- catto_mean(df_fact, tidyselect::one_of(c("x1", "x2")))
  mean_fact6 <- catto_mean(df_fact, tidyselect::one_of("x1", "x2"))
  mean_char1 <- catto_mean(df_char, response = "y")
  mean_char2 <- catto_mean(df_char, x1, x2, response = y)
  mean_char3 <- catto_mean(df_char, c(x1, x2), response = y)
  mean_char4 <- catto_mean(df_char, c("x1", "x2"), response = "y")
  mean_char5 <- catto_mean(df_char, tidyselect::one_of(c("x1", "x2")))
  mean_char6 <- catto_mean(df_char, tidyselect::one_of("x1", "x2"))

  expected_df_both <- data.frame(y = y,
                                 x1 = c(8.5, 5, NA, 5, 8.5),
                                 x2 = c(7 / 3, 7 / 3, 7 / 3, 12, 12))
  expected_both <- as.matrix(expected_df_both)
  result_names <- c(paste0("mean_fact", seq_len(6)), paste0("mean_char", seq_len(6)))
  char_and_fact <- mget(result_names)

  for (m in char_and_fact) {
    expect_equal(m, expected_both)
  }

  ### SUBSET OF CATEGORICAL COLUMNS ###

  expected_x1_only <- data.frame(y, x1 = c(8.5, 5, NA, 5, 8.5), x2)
  char_and_bare <- list(catto_mean(df_fact, "x1", response = "y"),
                        catto_mean(df_fact, x1, response = y),
                        catto_mean(df_fact,
                                   tidyselect::one_of("x1"),
                                   response = "y"))

  for (result in char_and_bare) {
    expect_equal(result, expected_x1_only)
  }

  ### RESPONSE NOT SPECIFIED ###

  expect_message(no_response <- catto_mean(df_fact, x1),
                 paste("`response` not supplied; using first column",
                       "'y' as the response variable."))
  expect_equal(no_response, expected_x1_only)

})


test_that("catto_mean correctly encodes test data.", {
  expected_df_both <- data.frame(y = y,
                                 x1 = c(8.5, 5, NA, 5, 8.5),
                                 x2 = c(7 / 3, 7 / 3, 7 / 3, 12, 12))
  expect_equal(catto_mean(df_fact, test = test_df),
               list(train = as.matrix(expected_df_both), test = as.matrix(encoded_test)))

})
###