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

to_factor <- function(.x) {
  if (is.factor(.x)) {
    if (anyNA(.x)) {
      levels(.x) <- append(levels(.x), "NA")
      .x[is.na(.x)] <- "NA"
    }
    .x
  } else {
    .x[is.na(.x)] <- "NA"
    factor(.x)
  }
}

####################
### model_matrix ###
####################

model_matrix <- function(.df, .cols = names(.df)) {
  .df[.cols] <- lapply(.df[.cols], to_factor)
  form <- make_form(.cols)
  mf <- stats::model.frame(form, data = .df)
  cs <- lapply(.df[.cols], contrasts, contrasts = FALSE)
  mm <- stats::model.matrix(form, mf, contrasts.arg = cs)
  attr(mm, "contrasts") <- NULL
  attr(mm, "assign") <- NULL
  rownames(mm) <- NULL
  mm
}

####################
### catto_onehot ###
####################

catto_onehot <- function(train, ..., test) {

  validate_col_types(train)
  test_also <- ! missing(test)
  if (test_also) check_train_test(train, test)

  cats <- pick_cols(train, ...)

  df_cat <- train[cats]
  df_keep <- train[setdiff(names(train), cats)]

  rm(train)

  df_cat <- model_matrix(df_cat)

  expanded <- cbind(df_keep, df_cat)

  mat_or_df(expanded)

}

###