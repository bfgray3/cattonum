##########################
### encode_no_new_cols ###
##########################

encode_no_new_cols <- function(fun = mean) {

  function(df, response, ...) {
    nms <- names(df)

    if (missing(response)) {
      response <- nms[1]
      message("`response` not supplied; using first column '",
              response, "' as the response variable.")
    }

    f <- function(column) {
      stats::ave(df[[response]], column, FUN = ignore_na(fun))
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