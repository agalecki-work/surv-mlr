# source("003-surv.glmnet-example.R")

rm(list=ls())

require(mlr3)
require(mlr3proba)
require(mlr3learners)
require(mlr3tuning)
require(mlr3verse)
require(glmnet)

source("./data/simdf1.R") # df created


# Create a survival task
task <- TaskSurv$new("survival_task", backend = df, time = "time", event ="status")
sort(names(task)) #  ".__enclos_env__" -- "weights"
head(task$truth(rows = NULL)) #  1.543+, 0.967, ...


# Perform initial split
# initial_split <- rsmp("holdout")
# initial_split$instantiate(task)

# Simple train/test split
# https://mlr3.mlr-org.com/reference/partition.html
 
splitx = partition(task, ratio= 0.7) # indices for "train", "test", "validation" 
names(splitx)

  at$train(task, row_ids = part$train)
 


# Separate the data into training and testing sets

train_task <- task$clone()$filter(initial_split$train_set(1))
test_task <- task$clone()$filter(initial_split$test_set(1))


# Load the glmnet learner
learner <- lrn("surv.glmnet", family= "cox")

# Define the hyperparameter search space
search_space <- ps(
  alpha = p_dbl(lower = 0, upper = 1),
  s = p_dbl(lower = 0.0001, upper = 0.1, logscale = TRUE)
)

# Define the resampling strategy
resampling <- rsmp("cv", folds = 5)

# Define the performance measure
measure <- msr("surv.cindex")

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
at$train(task)

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
at$train(train_task)

# Using the function to extract the best parameters
best_params <- check_best_params(at)
print(best_params)

# Check if best_params is not NULL

  # Extract the tuned learner (best_params is not null)
  tuned_learner <- at$learner

# Extract the glmnet model
glmnet_model <- tuned_learner$model

# Extract beta coefficients at the optimal lambda
## best_lambda <- glmnet_model$lambda.min  # This is automatically selected by glmnet
best_lambda <- best_params$s
beta_coefficients <- coef(glmnet_model$model, s = best_lambda)
print(beta_coefficients)

# Predict on the test set
predictions <- tuned_learner$predict(test_task)

# Print test set performance
performance <- predictions$score(msr("surv.cindex"))
print(performance)

