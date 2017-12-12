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

catto_mean <- function(dataframe,
                       ...,
                       response,
                       rm_na = TRUE,
                       verbose = TRUE) {

  validate_col_types(dataframe)

  nms <- names(dataframe)

  if (missing(response)) {
    response <- nms[1L]
    if (verbose) {
      message("`response` not supplied; using first column '",
              response, "' as the response variable.")
    }
  } else {
    response <- tidyselect::vars_select(nms, !! rlang::enquo(response))
  }

  cats <- pick_cols(dataframe, ...)
  dataframe[cats] <- lapply(dataframe[cats],
                            ave_na,
                            .x = dataframe[[response]],
                            .f = function(...) mean(..., na.rm = rm_na))

  mat_or_df(dataframe)

}

##################
### catto_freq ###
##################

catto_freq <-  function(dataframe, ..., verbose = TRUE) {

  validate_col_types(dataframe)

  nms <- names(dataframe)

  cats <- pick_cols(dataframe, ...)
  dataframe[cats] <- lapply(dataframe[cats],
                            ave_na,
                            .x = dataframe[[1L]],
                            .f = length)

  mat_or_df(dataframe)

}

###