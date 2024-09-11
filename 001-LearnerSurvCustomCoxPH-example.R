# source("000x.R")
require(survival)
require(mlr3)
require(mlr3proba)

# List objects exported from `ml3proba` package
exported_objects_proba <- getNamespaceExports("mlr3proba")
print(exported_objects_proba)

# Simulated data

set.seed(123)
mydata <- data.frame(
  time = rexp(100),
  status = sample(0:1, 100, replace = TRUE),
  predictor1 = rnorm(100),
  predictor2 = rnorm(100),
  weights = runif(100)
)

#-- Custom Learner that suports weights
require(mlr3learners)
require(R6)


LearnerSurvCustomCoxPH <- R6Class("LearnerSurvCustomCoxPH",
  inherit = mlr3proba::LearnerSurvCoxPH,
  public = list(
    initialize = function() {
      super$initialize()
    },

    train_internal = function(task) {
      # Extract data
      data <- task$data()
      # Extract weights
      weights <- data$weights
      # Drop weights from data
      data <- data[, colnames(data) != "weights", drop = FALSE]

      # Define survival object
      surv_obj <- survival::Surv(time = data$time, event = data$status)
      # Fit the Cox model with weights
      model <- survival::coxph(surv_obj ~ ., data = data, weights = weights)
      self$model <- model
    }
  )
)

# Register the custom learner
learner <- LearnerSurvCustomCoxPH$new()


#-- Create the Survival Task
# Specify the columns for time and event
task <- TaskSurv$new(id = "case_cohort_task", backend = mydata, time = "time", event = "status")

#  Define Resampling Strategy
resampling <- rsmp("cv", folds = 5)

# Perform the resampling
resample_model <- resample(task, learner, resampling)

# Aggregate results
performance <- resample_model$aggregate(msr("surv.cindex"))
print(performance)
