# source("004-surv.glmnet-spline-example2.R")

rm(list=ls())

require(mlr3)
require(mlr3proba)
require(mlr3learners)
require(mlr3tuning)
require(mlr3pipelines)
require(mlr3verse)
require(mlr3viz)
# require(mlr3fda)

require(mlr3verse)
require(survival)
require(glmnet)
require(splines)
sessionInfo()

# Simulate a regression dataset
set.seed(123)
n <- 100
p <- 3
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
time <- rexp(n, rate = 1)
status <- sample(0:1, n, replace = TRUE)
df <- as.data.frame(X)
df$time <- time
df$status <- status


#---- Create a survival task
task <- TaskSurv$new("survival_task", backend = df, time = "time", event ="status")
task

#---- Perform initial split
initial_split <- rsmp("holdout")
initial_split$instantiate(task)

# Separate the data into training and testing sets

train_task <- task$clone()$filter(initial_split$train_set(1))
test_task  <- task$clone()$filter(initial_split$test_set(1))


#---- Load the glmnet learner
learner <- lrn("surv.glmnet")

#---- Define the hyperparameter search space

search_space <- ps(
 alpha  = p_dbl(lower = 0, upper = 1),
 lambda = p_dbl(lower = 0.0001, upper = 0.1, logscale = TRUE)
)

#---- Create a Pipeline Using mlr3fda for Splines Transformation
# library(paradox)

# Define a function to apply splines transformation
apply_splines <- function(x) {
  as.data.table(splines::ns(x, df = 3))
}

# Define the pipeline graph for applying splines transformation
graph <- gunion(list(
  po("colapply", id = "spline_V1", applicator = apply_splines, affect_columns = selector_name("V1")),
  po("colapply", id = "spline_V2", applicator = apply_splines, affect_columns = selector_name("V2")),
  po("colapply", id = "spline_V3", applicator = apply_splines, affect_columns = selector_name("V3"))
)) %>>%
  po("featureunion") %>>%
  learner


# Create the pipeline learner
pipeline <- GraphLearner$new(graph)

#---- Tuning

#--- Define the resampling strategy for tuning
resampling <- rsmp("cv", folds = 5)

# Define the performance measure for survival analysis
measure <- msr("surv.cindex")

# Create the tuner
tuner <- tnr("grid_search", resolution = 5)  # or "random_search", "bayesopt", etc.

# Define the AutoTuner
at <- AutoTuner$new(
  learner = pipeline,
  resampling = resampling,
  measure = measure,
  search_space = search_space,
  terminator = trm("evals", n_evals = 20),  # use a smaller number for quick testing
  tuner = tuner
)


# Train the AutoTuner on the training set
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
}

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

