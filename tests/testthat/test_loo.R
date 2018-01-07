context("leave-one-out encoding")

#############
### SETUP ###
#############

resp_name <- "y"
x1 <- c("a", "b", NA, "b", "a")
x2 <- c("c", "c", "c", "d", "d")
y <- seq_along(x1)

df_fact <- data.frame(y, x1, x2)
df_char <- data.frame(y, x1, x2, stringsAsFactors = FALSE)

expected_df_both <- data.frame(y = y,
                               x1 = c(5, 4, NA, 2, 1),
                               x2 = c(2.5, 2, 1.5, 5, 4))
expected_both <- as.matrix(expected_df_both)

##################
### TRAIN DATA ###
##################

test_that("catto_loo correctly encodes train data.", {

  ### ALL CATEGORICAL COLUMNS ###

  loo_fact <- catto_loo(df_fact, response = resp_name)
  loo_char <- catto_loo(df_char, response = resp_name)

  expect_equal(loo_fact, expected_both)
  expect_equal(loo_char, expected_both)

  ### SUBSET OF CATEGORICAL COLUMNS ###

  expected_x1_only <- data.frame(y, x1 = c(5, 4, NA, 2, 1), x2)
  char_and_bare <- list(catto_loo(df_fact, "x1", response = "y"),
                        catto_loo(df_fact, x1, response = y),
                        catto_loo(df_fact,
                                   tidyselect::one_of("x1"),
                                   response = "y"))

  for (result in char_and_bare) expect_equal(result, expected_x1_only)

  ### RESPONSE NOT SPECIFIED ###

  expect_message(no_response <- catto_loo(df_fact, x1),
                 paste("`response` not supplied; using first column",
                       "'y' as the response variable."))
  expect_equal(no_response, expected_x1_only)

})

##################
### TEST DATA ###
##################

test_that("catto_loo correctly encodes test data.", {

  test_df <- data.frame(y = y,
                        x1 = c(NA, NA, "a", "b", "b"),
                        x2 = c("d", NA, NA, "c", "c"))

  encoded_test <- data.frame(y = y,
                             x1 = c(NA, NA, 3, 3, 3),
                             x2 = c(4.5, NA, NA, 2, 2))

  expect_equal(catto_loo(df_fact, test = test_df),
               list(train = expected_both, test = as.matrix(encoded_test)))

})
###