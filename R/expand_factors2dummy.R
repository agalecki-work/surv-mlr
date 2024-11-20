expand_factor_to_dummy <- function(data, factor_var) {
  # Ensure the input data is a data frame
  if (!is.data.frame(data)) {
    stop("Input data must be a data frame")
  }
  
  # Ensure the factor_var is a character string
  if (!is.character(factor_var) || length(factor_var) != 1) {
    stop("factor_var must be a single character string")
  }
  
  # Ensure the factor_var exists in the data frame
  if (!factor_var %in% colnames(data)) {
    stop(paste("Column", factor_var, "not found in the data frame"))
  }
  
  # Ensure the specified variable is a factor; if it's numeric or character, convert it to a factor
  if (!is.factor(data[[factor_var]])) {
    data[[factor_var]] <- as.factor(data[[factor_var]])
  }
  
  # Use model.matrix to create dummy variables
  dummy_matrix <- model.matrix(~ data[[factor_var]] + 0)
  
  # Convert to a data frame and set proper column names with prefix
  dummy_df <- as.data.frame(dummy_matrix)
  colnames(dummy_df) <- paste(factor_var, levels(data[[factor_var]]), sep = "_")
  
  # Combine the original data with the dummy variables
  result_df <- cbind(data, dummy_df)
  
  return(result_df)
}

# Example usage:
# Create a sample data frame with numeric levels
df <- data.frame(ID = 1:4, Category = factor(c(1, 2, 1, 3)), stringsAsFactors = FALSE)

# Use the function
expanded_df <- expand_factor_to_dummy(df, "Category")

# Print the result
print(expanded_df)