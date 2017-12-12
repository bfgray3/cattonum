##############
### ave_na ###
##############

ave_na <- function(.grouping, .x, .f) {
  nas <- is.na(.grouping)
  summarized <- stats::ave(.x, .grouping, FUN = .f)
  replace(summarized, nas, NA)
}

##################
### catto_mean ###
##################

catto_mean <- function(train,
                       ...,
                       response,
                       rm_na = TRUE,
                       verbose = TRUE) {

  validate_col_types(train)

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
  train[cats] <- lapply(train[cats],
                            ave_na,
                            .x = train[[response]],
                            .f = function(...) mean(..., na.rm = rm_na))

  mat_or_df(train)

}

##################
### catto_freq ###
##################

catto_freq <-  function(train, ..., verbose = TRUE) {

  validate_col_types(train)

  nms <- names(train)

  cats <- pick_cols(train, ...)
  train[cats] <- lapply(train[cats],
                            ave_na,
                            .x = train[[1L]],
                            .f = length)

  mat_or_df(train)

}

###