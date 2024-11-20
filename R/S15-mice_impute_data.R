mice_impute_data <- function(data, target_cols) {
  setDT(data)  # Ensure data is a data.table
  
  # Exclude target columns from data to be imputed and convert to data frame for mice
  data_df <- as.data.frame(data[, !target_cols, with = FALSE])
  #message("\nData structure before imputation:")
  #print(str(data_df))

  # Perform imputation using mice
  imputed_data <- mice(data_df, m = 1, maxit = 5, method = 'pmm', seed = 123)
  #message("\nStructure of imputed_data:")
  #print(str(imputed_data))
  
  # Complete the imputed data and convert back to data.table
  completed_data <- as.data.table(complete(imputed_data))
  #message("\nStructure of completed_data after imputation (excluding target columns):")
  #print(str(completed_data))
  
  # Reincorporate the target columns back to the completed data
  completed_data[, (target_cols) := data[, target_cols, with=FALSE]]
  #message("\nFinal completed_data structure after reintegrating target columns:")
  #print(str(completed_data))
  
  return(completed_data)
}


Skip_code <- function(x){
# Create lungx dataset from survival::lung and modify status column
lungx <- within(survival::lung, {
  status1 = as.numeric(status == 2)
  rm(status)
})
setDT(lungx)

# Use the imputation function on lungx dataset, indicating time and status1 as the target columns
completed_data <- mice_impute_data(lungx, c("time", "status1"))

# Display the dataset after imputation
print(completed_data)
}
