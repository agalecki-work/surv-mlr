
rm(list = ls())

# Load required libraries
library(mlr3)
library(mlr3learners)
library(mlr3extralearners) # Contains surv.glmnet
library(mlr3pipelines)
library(mlr3proba) # Contains TaskSurv
library(survival)
library(mlr3measures)

# Define the task using the lung dataset
task = TaskSurv$new(id = "lung", backend = survival::lung, time = "time", event = "status")
task

# Create toy test dataset
test_data <- data.frame(
  inst = c(1, 2, 3),
  time = c(306, 455, 1010),
  status = c(2, 1, 1),
  age = c(74, 68, 56),
  sex = c(1, 1, 2),
  ph.ecog = c(1, NA, 2),
  ph.karno = c(85, 70, NA),
  pat.karno = c(NA, NA, 70),
  meal.cal = c(2000, 2700, 2110),
  wt.loss = c(NA, 10, 5)
)

# Create the imputation pipeline
po_impute_median = po("imputemedian")
po_impute_mode = po("imputemode")
imputer = po_impute_median %>>% po_impute_mode

# Set up the learner
learner = lrn("surv.glmnet")
learner

# Combine the imputation pipeline and the learner into a single pipeline
graph = imputer %>>% learner
graph_learner = GraphLearner$new(graph)
graph_learner

# Resampling strategy for cross-validation
rsmp = rsmp("cv", folds = 5)
resample_result = resample(task, graph_learner, rsmp)

# Extract the optimal lambda
cv_models = lapply(resample_result$learners, function(l) l$model)
optimal_lambdas = sapply(cv_models, function(m) m$lambda.min)
optimal_lambda = median(optimal_lambdas, na.rm = TRUE)
print(optimal_lambda)

# Set the optimal lambda for the learner
learner$param_set$values$s = optimal_lambda
print(optimal_lambda)


# Train the model on the task
graph_learner$train(task)

# Create a new task for prediction on toy test data
test_task <- TaskSurv$new(id = "test_lung", backend = test_data, time = "time", event = "status")

# Perform prediction
prediction = graph_learner$predict(test_task)

# Evaluate the model
cindex = msr("surv.cindex")
cindex_score = cindex$score(prediction)
print(cindex_score)

