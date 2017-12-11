###################
### make_labels ###
###################

make_labels <- function(.x) {

  if (is.factor(.x)) .x <- as.character(.x)
  ordered_labs <- ordered_by_freq(.x)
  lkp <- data.frame(new_lab = seq_along(ordered_labs),
                    row.names = ordered_labs)
  lkp[.x, ]

}

###################
### catto_label ###
###################

catto_label <- function(dataframe, ..., response, verbose = TRUE) {

  validate_col_types(dataframe)

  nms <- names(dataframe)

  cats <- pick_cols(dataframe, ...)
  dataframe[cats] <- lapply(dataframe[cats], make_labels)

  mat_or_df(dataframe)

}

###