# this is a hack to stop the false positive note
# Namespace in Imports field not imported from: 'purrr'
#' @importFrom purrr partial
NULL


# TODO: more flexibility for NA handling? i.e. don't automatically remove?

aggregate_labeler <- function(.grouping, .x, .f) {
  summarized <- stats::ave(.x, .grouping, FUN = .f)
  non_repeat <- !(duplicated(.grouping) | is.na(.grouping))
  data.frame(
    new_lab = summarized[non_repeat],
    row.names = .grouping[non_repeat]
  )
}


#' Aggregate function encoding
#'
#' @param train The training data, in a `data.frame` or `tibble`.
#' @param ... The columns to be encoded.  If none are specified, then
#'   all character and factor columns are encoded.
#' @param aggregate_fun The aggregate function to be applied to the response
#'   variable for the rows belonging to the relevant level of the categorical
#'   predictor.  Takes a vector and returns a length one vector.
#' @param response The response variable used to calculate aggregate summaries.
#' @param test The test data, in a `data.frame` or `tibble`.
#' @param verbose Should informative messages be printed?  Defaults to
#'   `TRUE`.
#' @return The encoded dataset in a `cattonum_df` if no test dataset was
#'   provided, and the encoded datasets in a `cattonum_df2` otherwise.
#' @examples
#' catto_aggregate(iris, aggregate_fun = max, response = Sepal.Length)
#' @export
catto_aggregate <- function(train,
                            ...,
                            aggregate_fun,
                            response = NULL,
                            test = NULL,
                            verbose = TRUE) {
  UseMethod("catto_aggregate")
}

#' @export
# nolint start
catto_aggregate.data.frame <- function(train, ..., aggregate_fun, response = NULL, test = NULL, verbose = TRUE) {
  # nolint end
  validate_col_types(train)
  test_also <- !is.null(test)
  if (test_also) check_train_test(train, test)

  nms <- names(train)

  if (rlang::quo_is_null(enquo_response <- rlang::enquo(response))) {
    response <- nms[1L]
    if (verbose) {
      message(
        "`response` not supplied; using first column '",
        response, "' as the response variable."
      )
    }
  } else {
    response <- tidyselect::vars_select(nms, !!enquo_response)
  }

  cats <- pick_cols(train, deparse(substitute(train)), ...)

  center_lkps <- lapply(
    train[cats],
    aggregate_labeler,
    .x = train[[response]],
    .f = aggregate_fun
  )

  train[cats] <- encode_from_lkp(train[cats], center_lkps)

  if (!test_also) {
    cattonum_df(train)
  } else {
    test[cats] <- encode_from_lkp(test[cats], center_lkps)
    cattonum_df2(train = train, test = test)
  }
}


#' Mean encoding
#'
#' @param train The training data, in a `data.frame` or `tibble`.
#' @param ... The columns to be encoded.  If none are specified, then
#'   all character and factor columns are encoded.
#' @param response The response variable used to calculate means.
#' @param test The test data, in a `data.frame` or `tibble`.
#' @param verbose Should informative messages be printed?  Defaults to
#'   `TRUE`.
#' @return The encoded dataset in a `cattonum_df` if no test dataset was
#'   provided, and the encoded datasets in a `cattonum_df2` otherwise.
#' @examples
#' catto_mean(iris, response = Sepal.Length)
#' @export
catto_mean <- function(train, ..., response = NULL, test = NULL, verbose = TRUE) {
  UseMethod("catto_mean")
}

#' @export
# NOTE: tibbles and cattonum_dfs will dispatch here
# nolint start
catto_mean.data.frame <- partial(catto_aggregate, aggregate_fun = mean_cattonum)
# nolint end


#' Median encoding
#'
#' @param train The training data, in a `data.frame` or `tibble`.
#' @param ... The columns to be encoded.  If none are specified, then
#'   all character and factor columns are encoded.
#' @param response The response variable used to calculate medians.
#' @param test The test data, in a `data.frame` or `tibble`.
#' @param verbose Should informative messages be printed?  Defaults to
#'   `TRUE`.
#' @return The encoded dataset in a `cattonum_df` if no test dataset was
#'   provided, and the encoded datasets in a `cattonum_df2` otherwise.
#' @examples
#' catto_median(iris, response = Sepal.Length)
#' @export
catto_median <- function(train, ..., response = NULL, test = NULL, verbose = TRUE) {
  UseMethod("catto_median")
}

#' @export
# NOTE: tibbles and cattonum_dfs will dispatch here
# nolint start
catto_median.data.frame <- partial(
  # nolint end
  catto_aggregate,
  aggregate_fun = function(...) median(..., na.rm = TRUE)
)
