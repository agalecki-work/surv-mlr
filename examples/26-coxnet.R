rm(list=ls())

library(timereg)
library(cmprsk)
library(glmnet)
library(survival)
library(tidyverse)

# Load the built-in BMT data and prepare it
data(bmt)
data_df <- as_tibble(bmt) %>% rename(status = cause)

# Print the dataset information and status table
print(data_df, n = 10)
print(table(data_df$status))

# Number of rows
n <- nrow(data_df)

# Split the data into training and testing sets
set.seed(123)
train_index <- sample(seq_len(n), size = 0.7 * n)
train_data <- data_df[train_index, ]
test_data <- data_df[-train_index, ]

# Convert status to integer and factor for compatibility
train_data$status <- as.factor(as.character(train_data$status))
test_data$status <- as.factor(as.character(test_data$status))

# Use glmnet to perform elastic net regression
X_train <- as.matrix(train_data[, -(1:2)])
y_train <- train_data$status

# Fit the penalized regression model using glmnet for feature selection
cvfit <- cv.glmnet(X_train, y_train, family = "multinomial", alpha = 0.5)  # Elastic net

# Plot the cross-validated mean squared error to visualize lambda selection
plot(cvfit)

# Extract the best lambda value
best_lambda <- cvfit$lambda.min
print(best_lambda)

# Fit the penalized regression model with the best lambda on the training data
final_model <- glmnet(X_train, y_train, family = "multinomial", alpha = 0.5, lambda = best_lambda)

# Extract non-zero coefficients indicating significant predictors
coef_list <- coef(final_model, s = best_lambda)
selected_variables <- unique(unlist(lapply(coef_list, function(x) rownames(x)[which(x != 0)]) ) )
selected_variables <- selected_variables[selected_variables != "(Intercept)"]  # Remove intercept

print(selected_variables)

# Subset the training data to include only the selected variables
train_data_selected <- train_data[, c("time", "status", selected_variables)]

# Fit competing risks model on the reduced training data
crr_obj <- crr(ftime = as.numeric(train_data_selected$time), fstatus = as.numeric(train_data_selected$status),
               cov1 = as.matrix(train_data_selected[, selected_variables]))

# View summary of competing risks model
summary(crr_obj)

# Predict on the test data using the final penalized regression model
X_test <- as.matrix(test_data[, selected_variables])
predicted_risk <- predict(final_model, newx = X_test, type = "response")

# Print the predicted risk scores for the test data
print(predicted_risk)

# Example to predict cumulative incidence functions (CIFs) and visualize for first test subject
test_data_selected <- test_data[, c("time", "status", selected_variables)]

# Create CIF prediction for the first subject in the test set
cif_pred <- predict(crr_obj, cov1 = as.matrix(test_data_selected[1, selected_variables]), prob = TRUE)
plot(cif_pred, main = "Cumulative Incidence Function for First Test Subject", xlab = "Time", ylab = "Probability")
