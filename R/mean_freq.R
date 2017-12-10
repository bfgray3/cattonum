##############
### ave_na ###
##############

ave_na <- function(.grouping, .x, .f) {
  nas <- is.na(.grouping)
  summarized <- stats::ave(.x, .grouping, FUN = .f)
  replace(summarized, nas, NA)
}

##########################
### encode_no_new_cols ###
##########################

encode_no_new_cols <- function(.fun = function(...) mean(..., na.rm = TRUE)) {

  function(df, ..., response, verbose = TRUE) {

    validate_cols(df)

    nms <- names(df)

    if (missing(response)) {
      response <- nms[1L]
      if (verbose) {
        message("`response` not supplied; using first column '",
                response, "' as the response variable.")
      }
    } else {
      subs_resp <- substitute(response)
      response <- if (is.name(subs_resp)) deparse(subs_resp) else subs_resp
    }

    cats <- pick_cols(df, ...)
    df[cats] <- lapply(df[cats], ave_na, .x = df[[response]], .f = .fun)

    mat_or_df(df)

  }

}

##################
### catto_mean ###
##################

catto_mean <- encode_no_new_cols()

##################
### catto_freq ###
##################

catto_freq <- encode_no_new_cols(.fun = length)

###