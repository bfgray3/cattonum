######################
### center_labeler ###
######################

center_labeler <- function(.grouping, .x, .f) {
  summarized <- stats::ave(.x, .grouping, FUN = .f)
  non_repeat <- ! (duplicated(.grouping) | is.na(.grouping))
  data.frame(new_lab = summarized[non_repeat],
             row.names = .grouping[non_repeat])
}

###################
### mean_median ###
###################

mean_median <- function(.center_f) {

  function(train, ..., response, test, verbose = TRUE) {

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

    center_lkps <- lapply(train[cats],
                          center_labeler,
                          .x = train[[response]],
                          .f = .center_f)

    train[cats] <- encode_from_lkp(train[cats], center_lkps)

    if (! test_also) {
      train
    } else {
      test[cats] <- encode_from_lkp(test[cats], center_lkps)
      list(train = train, test = test)
    }

  }

}

##################
### catto_mean ###
##################

#' Mean encoding
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
#' catto_mean(iris, response = Sepal.Length)
#' @export
catto_mean <- mean_median(mean_cattonum)

####################
### catto_median ###
####################

#' Median encoding
#'
#' @param train The training data, in a \code{data.frame} or \code{tibble}.
#' @param ... The columns to be encoded.  If none are specified, then
#'   all character and factor columns are encoded.
#' @param response The response variable used to calculate medians.
#' @param test The test data, in a \code{data.frame} or \code{tibble}.
#' @param verbose Should informative messages be printed?  Defaults to
#'   \code{TRUE}.
#' @return The encoded dataset in a \code{data.frame} or \code{tibble},
#'   whichever was input.  If a test dataset was provided, a list with names
#'   "train" and "test" is returned holding the encoded training and
#'   test datasets.
#' @examples
#' catto_median(iris, response = Sepal.Length)
#' @export
catto_median <- mean_median(function(...) median(..., na.rm = TRUE))
