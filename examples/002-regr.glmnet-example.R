# source("002-regr.glmnet-example.R")

rm(list=ls())

require(mlr3)
require(mlr3learners)
require(mlr3tuning)
require(mlr3verse)
require(glmnet)


# Simulate a regression dataset
set.seed(123)
n <- 100
p <- 20
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
y <- rnorm(n)
df <- as.data.frame(X)
df$target <- y

# Create a regression task
task <- TaskRegr$new("regression_task", backend = df, target = "target")

# Perform initial split
initial_split <- rsmp("holdout")
initial_split$instantiate(task)

# Separate the data into training and testing sets

train_task <- task$clone()$filter(initial_split$train_set(1))
test_task <- task$clone()$filter(initial_split$test_set(1))


# Load the glmnet learner
learner <- lrn("regr.glmnet")

# Define the hyperparameter search space
search_space <- ps(
  alpha = p_dbl(lower = 0, upper = 1),
  s = p_dbl(lower = 0.0001, upper = 0.1, logscale = TRUE)
)

# Define the resampling strategy
resampling <- rsmp("cv", folds = 5)

# Define the performance measure
measure <- msr("regr.rmse")

# Create the tuning instance
tuner <- tnr("grid_search", resolution = 10)  # or "random_search", "bayesopt", etc.

# Define the AutoTuner
at <- AutoTuner$new(
  learner = learner,
  resampling = resampling,
  measure = measure,
  search_space = search_space,
  terminator = trm("evals", n_evals = 50),
  tuner = tuner
)

# Train the AutoTuner
at$train(train_task)

# Inspect the tuning archive for an overview of tuning results
tuning_archive <- as.data.table(at$archive)
## print(tuning_archive)

check_best_params <- function(at) {
  # Function to check and extract the best parameters
  # Ensure archives are available

  # Check the best parameters
if (!is.null(at$archive)) {
  best_params <- at$archive$best()
  if (!is.null(best_params)) {
    return(best_params)
  } else {
    warning("Tuning results are empty. Please check the setup.")
  }
} else {
  stop("Tuning did not produce any results. Please check the setup.")
}
} # function

# Train the AutoTuner
at$train(task)

# Using the function to extract the best parameters
best_params <- check_best_params(at)
print(best_params)

# Check if best_params is not NULL

  # Extract the tuned learner
  tuned_learner <- at$learner


# Extract the glmnet model
glmnet_model <- tuned_learner$model

# Extract beta coefficients at the optimal lambda
best_lambda <- glmnet_model$lambda.min  # This is automatically selected by glmnet
beta_coefficients <- coef(glmnet_model, s = best_lambda)
print(beta_coefficients)

# Predict on the test set
predictions <- tuned_learner$predict(test_task)

# Print test set performance
performance <- predictions$score(msr("regr.rmse"))
print(performance)

