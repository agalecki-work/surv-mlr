select_columns_data <- function(data, id_col, time_col, status_col, keep_columns, weight_col = NULL, traceit = FALSE) {
  data <- as.data.table(data)
  print(table(data[[status_col]])) 
  required_columns <- c(id_col, time_col, status_col)
  if (length(required_columns) < 3) stop("Some of the mandatory id/time/status column names not provided")
  if (is.null(predictor_columns) || length(predictor_columns) == 0) stop("`predictor_columns` argument has length zero")
  if (!all(predictor_columns %in% names(data))) stop("Some specified predictor columns are not present in the dataset")

  present_required_columns <- intersect(required_columns, names(data))
  selected_columns <- unique(c(predictor_columns, present_required_columns, weight_col))
  data <- data[, ..selected_columns]
  return(data)
}