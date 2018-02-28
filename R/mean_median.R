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

mean_median <- function(.tr, ..., .r, .te, .fn, .v) {
  
  validate_col_types(.tr)
  test_also <- ! missing(.te)
  if (test_also) check_train_test(.tr, .te)
  
  nms <- names(.tr)
  
  if (missing(.r)) {
    .r <- nms[1L]
    if (.v) {
      message("`response` not supplied; using first column '",
              .r, "' as the response variable.")
    }
  } else {
    .r <- tidyselect::vars_select(nms, !! .r)
  }
  
  cats <- pick_cols(.tr, ...)
  
  center_lkps <- lapply(.tr[cats],
                        center_labeler,
                        .x = .tr[[.r]],
                        .f = function(...) .fn(..., na.rm = TRUE))
  
  .tr[cats] <- encode_from_lkp(.tr[cats], center_lkps)
  
  if (! test_also) {
    .tr
  } else {
    .te[cats] <- encode_from_lkp(.te[cats], center_lkps)
    list(train = .tr, test = .te)
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
#'   whichever was input.  If a test dataset was provided, a named list
#'   is returned holding the encoded training and test datasets.
#' @examples
#' catto_mean(iris, response = Sepal.Length)
#' @export
catto_mean <- function(train,
                       ...,
                       response,
                       test,
                       verbose = TRUE) {

  if (!missing(response)){
    mean_median(.tr = train, 
                ..., 
                .r = tidyselect::vars_select(names(train), !! dplyr::enquo(response)),
                .te = test, 
                .fn = mean, 
                .v = verbose)
  } else {
    mean_median(.tr = train, 
                ...,
                .te = test, 
                .fn = mean, 
                .v = verbose)
  }
  
}

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
#'   whichever was input.  If a test dataset was provided, a named list
#'   is returned holding the encoded training and test datasets.
#' @examples
#' catto_median(iris, response = Sepal.Length)
#' @export
catto_median <- function(train,
                         ...,
                         response,
                         test,
                         verbose = TRUE) {
  
  if (!missing(response)){
    mean_median(.tr = train, 
                ..., 
                .r = tidyselect::vars_select(names(train), !! dplyr::enquo(response)),
                .te = test, 
                .fn = median, 
                .v = verbose)
  } else {
    mean_median(.tr = train, 
                ...,
                .te = test, 
                .fn = median, 
                .v = verbose)
  }
  
}

###