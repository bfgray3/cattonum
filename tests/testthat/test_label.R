context("label encoding")

#############
### SETUP ###
#############

y <- 2 ^ seq(from = 0, to = 5)
x1 <- c("a", "b", NA, "b", "a", "a")
x2 <- c("c", "c", "c", "c", "d", "d")

df_fact <- data.frame(y = y, x1 = x1, x2 = x2)
df_char <- data.frame(y = y, x1 = x1, x2 = x2, stringsAsFactors = FALSE)

test_df <- data.frame(y = y,
                      x1 = c(NA, NA, "a", "b", "b", "b"),
                      x2 = c("d", NA, NA, "c", "c", "c"))

encoded_test <- data.frame(y = y,
                           x1 = c(NA, NA, 2, 1, 1, 1),
                           x2 = c(1, NA, NA, 2, 2, 2))

incr_df <- data.frame(y = y,
                      x1 = c(2, 1, NA, 1, 2, 2),
                      x2 = c(2, 2, 2, 2, 1, 1))

decr_df <- data.frame(y = y,
                      x1 = c(1, 2, NA, 2, 1, 1),
                      x2 = c(1, 1, 1, 1, 2, 2))

order_df <- data.frame(y = y,
                       x1 = c(1, 2, NA, 2, 1, 1),
                       x2 = c(1, 1, 1, 1, 2, 2))

######################
### LABEL ENCODING ###
######################

test_that("catto_label works.", {

  order_options <- c("increasing",
                     "decreasing",
                     "observed",
                     "random")

  for (ordering_method in order_options) {

    ### ALL CATEGORICAL COLUMNS ###

    label_fact1 <- catto_label(df_fact,
                               ordering = ordering_method)
    label_fact2 <- catto_label(df_fact, x1, x2,
                               ordering = ordering_method)
    label_fact3 <- catto_label(df_fact, c(x1, x2),
                               ordering = ordering_method)
    label_fact4 <- catto_label(df_fact, c("x1", "x2"),
                               ordering = ordering_method)
    label_fact5 <- catto_label(df_fact, tidyselect::one_of(c("x1", "x2")),
                               ordering = ordering_method)
    label_fact6 <- catto_label(df_fact, tidyselect::one_of("x1", "x2"),
                               ordering = ordering_method)
    label_char1 <- catto_label(df_char,
                               ordering = ordering_method)
    label_char2 <- catto_label(df_char, x1, x2,
                               ordering = ordering_method)
    label_char3 <- catto_label(df_char, c(x1, x2),
                               ordering = ordering_method)
    label_char4 <- catto_label(df_char, c("x1", "x2"),
                               ordering = ordering_method)
    label_char5 <- catto_label(df_char, tidyselect::one_of(c("x1", "x2")),
                               ordering = ordering_method)
    label_char6 <- catto_label(df_char, tidyselect::one_of("x1", "x2"),
                               ordering = ordering_method)

    expected_df_both <- switch(ordering_method,
                               increasing = incr_df,
                               decreasing = decr_df,
                               observed = ,
                               random = order_df)

    expected_both <- as.matrix(expected_df_both)
    result_names <- c(paste0("label_fact", seq_len(6)), paste0("label_char", seq_len(6)))
    char_and_fact <- mget(result_names)

    if (ordering_method != "random") {
      for (m in char_and_fact) {
        expect_equal(m, expected_both)
      }
    } else {
      for (j in seq_len(ncol(expected_both))) {
        expect_equal(sort(m[ , 1], na.last = TRUE),
                     sort(expected_both[ , 1], na.last = TRUE))
      }
    }

    ### SUBSET OF CATEGORICAL COLUMNS ###

    expected_x1_only <- data.frame(y = y, x1 = c(2, 1, NA, 1, 2, 2), x2 = x2)
    char_and_bare <- list(catto_label(df_fact, "x1"),
                          catto_label(df_fact, x1),
                          catto_label(df_fact, tidyselect::one_of("x1")))

    for (result in char_and_bare) {

      expect_equal(result, expected_x1_only)

    }

  }

})

test_that("catto_label correctly encodes test data.", {

  expect_equal(catto_label(df_fact, test = test_df),
                 list(train = incr_df, test = encoded_test))
})

###