# source("15-surv-learners1.R")

# Load necessary libraries
library(data.table)
library(glmnet)
library(mlr3)
library(mlr3proba)
library(paradox)
library(survival)

load("./results/SurvTasks.Rdata", verbose=TRUE)
names(res_SurvTasks)

task = res_SurvTasks$task_train


# Define a custom survival learner using cv.glmnet
LearnerSurvCVGlmnet <- R6::R6Class("LearnerSurvCVGlmnet",
  inherit = mlr3proba::LearnerSurv,

  public = list(
    initialize = function() {
      ps <- paradox::ps(
        alpha            = p_dbl(lower = 0, upper = 1, default = 1),
        lambda.min.ratio = p_dbl(lower = 1e-5, upper = 1e-1, default = 1e-4)
      )
      super$initialize(
        id = "surv.cv_glmnet",
        packages = c("glmnet", "survival"),
        feature_types = c("numeric", "integer"),
        predict_types = c("crank", "lp"),
        param_set = ps
      )
    }
  ),

  private = list(
    .train = function(task) {
      x <- as.matrix(task$data(cols = task$feature_names))
      y <- survival::Surv(unlist(task$data(cols =task$target_names[1])), unlist(task$data(cols =task$target_names[2])))
      self$model <- cv.glmnet(x, y, family = "cox", alpha = self$param_set$values$alpha, lambda.min.ratio = self$param_set$values$lambda.min.ratio)
    },

    .predict = function(task) {
      x <- as.matrix(task$data(cols = task$feature_names))
      lp <- drop(predict(self$model, newx = x, s = "lambda.min", type = "link"))
      crank <- exp(lp)
      list(crank = crank, lp = lp)
    }
  )
)

# Instantiate the custom learner
learner <- LearnerSurvCvGlmnet$new()
print(learner)
# Train/Test Split
set.seed(42)  # for reproducibility
train_set <- sample(seq_len(task$nrow), 0.7 * task$nrow)
test_set <- setdiff(seq_len(task$nrow), train_set)

# Train the learner
learner$train(task, row_ids = train_set)
cat("??10")
# Predict on test data
prediction <- learner$predict(task, row_ids = test_set)

# Convert the predicted survival probabilities into a binary classification based on 5-year threshold
predicted_lp <- prediction$lp

# Define a threshold for 5-year survival (for demonstration purposes, use the median of training set predictions)
threshold <- quantile(predicted_lp, probs = 0.5) 

# Convert to binary outcome based on the threshold
binary_outcome <- ifelse(predicted_lp <= threshold, 1, 0)

# True binary outcome for evaluation
true_outcome <- task$truth(rows = test_set)
true_binary_outcome <- as.integer(true_outcome$time > 1825 & true_outcome$status == 1)

# Calculate accuracy
accuracy <- sum(binary_outcome == true_binary_outcome) / length(binary_outcome)
print(paste("Accuracy:", accuracy))