# Load necessary libraries
library(mlr3)
library(mlr3proba)
library(mlr3learners)
library(mlr3verse)
library(survival)
library(data.table)
source("./R/predict_risk_at_time.R")

# Load dataset
veteran <- as.data.table(survival::veteran)

# Create task
task = TaskSurv$new(id = "veteran", backend = veteran, time = "time", event = "status")

# Define and train the learner
learner = lrn("surv.coxph")
learner$train(task)

predictions = learner$predict(task)

# Using the function
t_star = 90  # Example time point
risk_scores = predict_risk_at_time(learner, task, t_star)
print(risk_scores)