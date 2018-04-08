#################
### make_form ###
#################

make_form <- function(.vars, .enc = c("dummy", "onehot")) {
  .enc_type <- match.arg(.enc)
  string <- paste("~",
                  paste(.vars, collapse = " + "),
                  if (.enc_type == "onehot") "- 1")
  stats::as.formula(string)
}

##################
### set_levels ###
##################

set_levels <- function(.f, .l) {
  factor(.f, levels = .l)
}

####################
### model_matrix ###
####################

model_matrix <- function(.df,
                         .enc_type = c("dummy", "onehot"),
                         .cols = names(.df),
                         .levels = NULL) {
  .enc_type <- match.arg(.enc_type)
  .df[.cols] <- lapply(.df[.cols], as.factor)
  if (! is.null(.levels)) {
    .df[.cols] <- Map(set_levels, .f = .df[.cols], .l = .levels)
  }
  form <- make_form(.cols, .enc_type)
  mf <- stats::model.frame(form, data = .df, na.action = stats::na.pass)
  if (.enc_type == "onehot") {
    cs <- lapply(.df[.cols], stats::contrasts, contrasts = FALSE)
    mm <- stats::model.matrix(form, mf, contrasts.arg = cs)
  } else {
    mm <- stats::model.matrix(form, mf)
    mm <- mm[ , colnames(mm) != "(Intercept)", drop = FALSE]
  }
  attr(mm, "contrasts") <- NULL
  attr(mm, "assign") <- NULL
  rownames(mm) <- NULL
  mm
}

####################
### na_new_levels ##
####################

na_new_levels <- function(.x, .orig_levels) {
  replace(.x, ! is.element(.x, .orig_levels), NA)
}

####################
### df_to_binary ###
####################

df_to_binary <- function(.df, .enc, .cats, .levs = NULL) {
  df_cat <- .df[.cats]
  df_keep <- .df[setdiff(names(.df), .cats)]
  rm(.df)
  df_cat <- model_matrix(df_cat, .enc_type = .enc, .levels = .levs)
  dplyr::bind_cols(df_keep, as.data.frame(df_cat))
}

####################
### dummy_onehot ###
####################

dummy_onehot <- function(.enc_type) {

  function(train, ..., test, verbose = TRUE) {

    validate_col_types(train)
    test_also <- ! missing(test)
    if (test_also) check_train_test(train, test)

    cats <- pick_cols(train, ...)

    train_expanded <- df_to_binary(train, .enc_type, cats)

    if (test_also) {
      train_levels <- lapply(train[cats], unique)
      test[cats] <- Map(na_new_levels,
                        .x = test[cats],
                        .orig_levels = train_levels)
      test_expanded <- df_to_binary(test, .enc_type, cats, train_levels)
    }

    if (! test_also) {
      train_expanded
    } else {
      list(train = train_expanded, test = test_expanded)
    }

  }

}

####################
### catto_onehot ###
####################

#' One-hot encoding
#'
#' @param train The training data, in a \code{data.frame} or \code{tibble}.
#' @param ... The columns to be encoded.  If none are specified, then
#'   all character and factor columns are encoded.
#' @param test The test data, in a \code{data.frame} or \code{tibble}.
#' @param verbose To be used in the future.
#' @return The encoded dataset in a \code{data.frame} or \code{tibble},
#'   whichever was input.  If a test dataset was provided, a named list
#'   is returned holding the encoded training and test datasets.
#' @examples
#' catto_onehot(iris, response = Sepal.Length)
#' @export
catto_onehot <- dummy_onehot("onehot")

###################
### catto_dummy ###
###################

#' Dummy encoding
#'
#' @param train The training data, in a \code{data.frame} or \code{tibble}.
#' @param ... The columns to be encoded.  If none are specified, then
#'   all character and factor columns are encoded.
#' @param test The test data, in a \code{data.frame} or \code{tibble}.
#' @param verbose To be used in the future.
#' @return The encoded dataset in a \code{data.frame} or \code{tibble},
#'   whichever was input.  If a test dataset was provided, a named list
#'   is returned holding the encoded training and test datasets.
#' @examples
#' catto_dummy(iris, response = Sepal.Length)
#' @export
catto_dummy <- dummy_onehot("dummy")

###