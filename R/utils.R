validate_col_types <- function(.df) {
  good_cols <- vapply(.df, is_cat_or_num, logical(1L))
  if (!all(good_cols)) {
    bad_cols <- names(.df)[!good_cols]
    bad_col_list <- colname_list(bad_cols)
    error_msg_verb <- if (length(bad_cols) > 1L) " are " else " is "
    stop("All columns must be numeric, character, logical, or factor. ",
      bad_col_list, error_msg_verb, "not.",
      call. = FALSE
    )
  }
}


is_cat <- function(.x) is.factor(.x) || is.character(.x) || is.logical(.x)


is_cat_or_num <- function(.x) {
  is.numeric(.x) || is_cat(.x)
}


colname_list <- function(.nms) {
  paste(paste0("'", .nms, "'"), collapse = ", ")
}


dots_to_char <- function(...) {
  deparse(substitute(...))
}


pick_cols <- function(.df, .df_name, ...) {
  if (identical(length(substitute(alist(...))), 1L)) { # FIXME: has to be a better way
    all_cats(.df)
  } else {
    col_spec <- dots_to_char(...)
    tryCatch(
      # FIXME: major hack!
      names(
        tidyselect::eval_select(rlang::expr(c(...)), .df[1L, , drop = FALSE])
      ),
      error = function(e) {
        stop(
          "'", col_spec, "' is not a valid column specification for ", .df_name, ".",
          call. = FALSE
        )
      }
    )
  }
}


all_cats <- function(.df) {
  nms <- names(.df)
  cats <- vapply(.df, is_cat, logical(1L))
  nms[cats]
}


ordered_by_freq <- function(.x, .decr = FALSE) {
  names(sort(table(.x), decreasing = .decr))
}


check_train_test <- function(.train, .test) {
  if (!identical(names(.train), names(.test))) {
    stop(
      "`train` and `test` data frames must have the same names.",
      call. = FALSE
    )
  }
}


expand_lkp <- function(.dat, .lookup_table) {
  .lookup_table[as.character(.dat), ]
}


encode_from_lkp <- function(.d, .l) {
  Map(expand_lkp, .dat = .d, .lookup_table = .l)
}
