##########################
### encode_no_new_cols ###
##########################

encode_no_new_cols <- function(fun = function(...) mean(..., na.rm = TRUE)) {

  function(df, ..., response, verbose = TRUE) {

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

    f <- function(column) {
      nas <- is.na(column)
      summarized <- stats::ave(df[[response]], column, FUN = fun)
      replace(summarized, nas, NA_real_)
    }

    cats <- pick_cols(df, ...)
    df[cats] <- lapply(df[cats], f)

    all_numerics <- all(vapply(df, is.numeric, logical(1)))

    if (all_numerics) as.matrix(df)
    else df

  }

}

##################
### catto_mean ###
##################

catto_mean <- encode_no_new_cols()

##################
### catto_freq ###
##################

catto_freq <- encode_no_new_cols(fun = length)

###