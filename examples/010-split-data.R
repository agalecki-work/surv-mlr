library(mlr3)
library(mice)
library(mlr3pipelines)
library(mlr3learners)

# Initial data with missing values
data <- data.frame(
  x1 = c(1, 2, NA, 4, 5, 2, NA, 3, NA, 6),
  x2 = as.factor(c("A", NA, "B", "A", NA, "B", "A", NA, "B", NA)),
  y = factor(c("A", "B", "A", NA, "A", "A", "B", "A", "B", NA))
)

# Split data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(seq_len(nrow(data)), size = 0.7 * nrow(data))
training_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Handle missing target variable in training data
training_data <- training_data[!is.na(training_data$y),] # rows with missing y omitted

# Impute training data using mice
imputed_train_mids <- mice(training_data, m = 1, maxit = 50, method = 'pmm', seed = 123)

# Check logged events
print(imputed_train_mids$loggedEvents)

# Extract the complete imputed data for the first imputed dataset
training_data_imputed <- complete(imputed_train_mids, 1)

# Create an mlr3 task with the imputed training data
task_train <- TaskClassif$new(id = "train_task", backend = training_data_imputed, target = "y")

# Choose a learner, e.g., decision tree
learner <- lrn("classif.rpart")

# Train the model on the imputed training task
learner$train(task_train)

# Defining predictor matrix explicitly (same logic as in `mice`) to ensure test data is handled consistently
pred_matrix <- imputed_train_mids$predictorMatrix

# Impute test data using the same strategy
imputed_test_mids <- mice(test_data, m = 1, maxit = 50, method = 'pmm', seed = 123, predictorMatrix = pred_matrix)

# Check logged events for test set imputation
print(imputed_test_mids$loggedEvents)

test_data_imputed <- complete(imputed_test_mids, 1)

# Handle missing target variable in the test data
test_data_imputed <- test_data_imputed[!is.na(test_data_imputed$y),]

# Create a classification task with the imputed test data
task_test <- TaskClassif$new(id = "test_task", backend = test_data_imputed, target = "y")

# Predict using the trained model on the imputed test dataset
prediction <- learner$predict(task_test)

# Print predictions
print(prediction)