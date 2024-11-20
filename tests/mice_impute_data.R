# Imputation function using mice
mice_impute_data <- function(data, target_cols, traceit = FALSE) {
  if (traceit) message("==== mice_impute_data() STARTS: target_cols (excluded from imputations)")

  setDT(data)  # Ensure the data is a data.table
  data_df <- as.data.frame(data[, !target_cols, with = FALSE])  # Convert to data.frame for mice
  imputed_data <- mice(data_df, m = 1, maxit = 5, method = 'pmm', seed = 123)
  completed_data <- as.data.table(complete(imputed_data))  # Convert back to data.table
  completed_data[, (target_cols) := data[, target_cols, with = FALSE]]  # Reincorporate target columns

  if (traceit) {
    message("--- mice_impute_data(): completed_data")
    print(str(completed_data))
    message("--- mice_impute_data(): ENDS")
  }

  return(completed_data)
}
