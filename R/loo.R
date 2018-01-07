###################
### loo_labeler ###
###################

loo_labeler <- function(.grp, .y) {
  r <- vapply(seq_along(.grp),
              function(.i) mean(.y[-.i][.grp[-.i] == .grp[.i]], na.rm = TRUE),
              numeric(1L))
  replace(r, is.nan(r), NA)
}

#################
### catto_loo ###
#################

catto_loo <- function(train,
                       ...,
                       response,
                       test,
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

  if (test_also) {
    # unneccessarily encodes training data with means
    test <- catto_mean(train, cats, response = response, test = test)[["test"]]
  }

  train[cats] <- lapply(train[cats], loo_labeler, .y = train[[response]])

  if (! test_also) {
    mat_or_df(train)
  } else {
    list(train = mat_or_df(train), test = test)
  }

}

###