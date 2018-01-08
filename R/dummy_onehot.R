#################
### make_form ###
#################

make_form <- function(.vars) {
  string <- paste("~", paste(.vars, collapse = " + "), "- 1")
  stats::as.formula(string)
}

##################
### set_levels ###
##################

# TODO: just use `levels<-`
set_levels <- function(.f, .l) {
  factor(.f, levels = .l)
}

####################
### model_matrix ###
####################

model_matrix <- function(.df, .cols = names(.df), .levels = NULL) {
  .df[.cols] <- lapply(.df[.cols], as.factor)
  if (! is.null(.levels)) {
    .df[.cols] <- Map(set_levels, .f = .df[.cols], .l = .levels)
  }
  form <- make_form(.cols)
  mf <- stats::model.frame(form, data = .df, na.action = na.pass)
  cs <- lapply(.df[.cols], stats::contrasts, contrasts = FALSE)
  mm <- stats::model.matrix(form, mf, contrasts.arg = cs)
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

df_to_binary <- function(.df, .cats, .levs = NULL) {
  df_cat <- .df[.cats]
  df_keep <- .df[setdiff(names(.df), .cats)]
  rm(.df)
  df_cat <- model_matrix(df_cat, .levels = .levs)
  cbind(df_keep, df_cat)
}

####################
### catto_onehot ###
####################

catto_onehot <- function(train, ..., test) {

  validate_col_types(train)
  test_also <- ! missing(test)
  if (test_also) check_train_test(train, test)

  cats <- pick_cols(train, ...)

  train_expanded <- df_to_binary(train, cats)

  if (test_also) {
    train_levels <- lapply(train[cats], unique)
    test[cats] <- Map(na_new_levels,
                      .x = test[cats],
                      .orig_levels = train_levels)
    test_expanded <- df_to_binary(test, cats, .levs = train_levels)
  }

  if (! test_also) {
    mat_or_df(train_expanded)
  } else {
    lapply(list(train = train_expanded, test = test_expanded), mat_or_df)
  }

}

###