###################
### loo_labeler ###
###################

loo_labeler <- function(.grp, .y) {
  vapply(seq_along(.grp),
         function(.i) mean_cattonum(.y[-.i][.grp[-.i] == .grp[.i]]),
         numeric(1L))
}

#################
### catto_loo ###
#################

#' Leave-one-out encoding
#'
#' @param train The training data, in a \code{data.frame} or \code{tibble}.
#' @param ... The columns to be encoded.  If none are specified, then
#'   all character and factor columns are encoded.
#' @param response The response variable used to calculate means.
#' @param test The test data, in a \code{data.frame} or \code{tibble}.
#' @param verbose Should informative messages be printed?  Defaults to
#'   \code{TRUE}.
#' @return The encoded dataset in a \code{data.frame} or \code{tibble},
#'   whichever was input.  If a test dataset was provided, a list with names
#'   "train" and "test" is returned holding the encoded training and
#'   test datasets.
#' @examples
#' catto_loo(iris, response = Sepal.Length)
#' @export
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
    response <- tidyselect::vars_select(nms, !! dplyr::enquo(response))
  }

  cats <- pick_cols(train, deparse(substitute(train)), ...)

  if (test_also) {
    # unneccessarily encodes training data with means
    test <- catto_mean(train, cats, response = response, test = test)[["test"]]
  }

  train[cats] <- lapply(train[cats], loo_labeler, .y = train[[response]])

  if (! test_also) {
    train
  } else {
    list(train = train, test = test)
  }

}
