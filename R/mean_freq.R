####################
### mean_labeler ###
####################

mean_labeler <- function(.grouping, .x, .f) {
  summarized <- stats::ave(.x, .grouping, FUN = .f)
  non_repeat <- ! (duplicated(.grouping) | is.na(.grouping))
  data.frame(new_lab = summarized[non_repeat],
             row.names = .grouping[non_repeat])
}

##################
### catto_mean ###
##################

catto_mean <- function(train,
                       ...,
                       response,
                       test,
                       rm_na = TRUE,
                       verbose = TRUE) {

  validate_col_types(train)
  test_also <- ! missing(test)
  if (test_also) check_train_test(train, test)

  nms <- names(train)

  if (missing(response)) {
    response <- nms[1L]
    if (verbose) {
      message("`response` not supplied; using first column '",
              response, "' as the response variable.")
    }
  } else {
    response <- tidyselect::vars_select(nms, !! rlang::enquo(response))
  }

  cats <- pick_cols(train, ...)

  mean_lkps <- lapply(train[cats],
                      mean_labeler,
                      .x = train[[response]],
                      .f = function(...) mean(..., na.rm = TRUE))

  train[cats] <- encode_from_lkp(train[cats], mean_lkps)

  if (! test_also) {
    mat_or_df(train)
  } else {
    test[cats] <- encode_from_lkp(test[cats], mean_lkps)
    lapply(list(train = train, test = test), mat_or_df)
  }

}

####################
### freq_labeler ###
####################

freq_labeler <- function(.grouping) {
  results <- as.data.frame(table(.grouping))
  rownames(results) <- results[[".grouping"]]
  results[names(results) != ".grouping"]
}

##################
### catto_freq ###
##################

catto_freq <-  function(train, ..., test, verbose = TRUE) {

  validate_col_types(train)
  test_also <- ! missing(test)
  if (test_also) check_train_test(train, test)

  nms <- names(train)

  cats <- pick_cols(train, ...)

  freq_lkps <- lapply(train[cats], freq_labeler)

  train[cats] <- encode_from_lkp(train[cats], freq_lkps)

  if (! test_also) {
    mat_or_df(train)
  } else {
    test[cats] <- encode_from_lkp(test[cats], freq_lkps)
    lapply(list(train = train, test = test), mat_or_df)
  }

}

###