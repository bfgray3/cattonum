#################
### make_form ###
#################

make_form <- function(.vars) {
  string <- paste("~", paste(.vars, collapse = " + "), "- 1")
  as.formula(string)
}

#################
### to_factor ###
#################

to_factor <- function(.column) {
  if (is.factor(.column)) {
    if (anyNA(.column)) {
      levels(.column) <- append(levels(.column), "NA")
      .column[is.na(.column)] <- "NA"
    }
    .column
  } else {
    .column[is.na(.column)] <- "NA"
    factor(.column)
  }
}

####################
### model_matrix ###
####################

model_matrix <- function(.df, .cols = names(.df)) {
  .df[.cols] <- lapply(.df[.cols], to_factor)
  form <- make_form(.cols)
  mm <- stats::model.matrix(form,
                            stats::model.frame(form, data = .df),
                            contrasts.arg = lapply(.df[.cols], contrasts, contrasts = FALSE))
  attr(mm, "contrasts") <- NULL
  attr(mm, "assign") <- NULL
  rownames(mm) <- NULL
  mm
}

####################
### catto_onehot ###
####################

catto_onehot <- function(df, ...) {

  validate_cols(df)

  cats <- pick_cols(df, ...)

  df_cat <- df[cats]
  df_keep <- df[setdiff(names(df), cats)]

  rm(df)

  df_cat <- model_matrix(df_cat)

  expanded <- cbind(df_keep, df_cat)

  mat_or_df(expanded)

}

###