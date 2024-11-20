# source("010-cv.glmnet-predict_risk_at_time.R")

# Load necessary libraries
rm(list=ls())


library(mlr3)
library(mlr3proba)
library(mlr3learners)
library(mlr3extralearners) 
library(mlr3verse)
library(glmnet)
library(data.table)
source("./R/predict_risk_at_time.R")

# Params

alphax = 0.9
time_nm = "time"
event_nm = "status"
nfolds  = 10

# Load dataset
vet = as.data.table(survival::veteran)
veteran = within(vet, {
    celltyp_num = as.numeric(celltype)
    rm(celltype)
    }) # Factors are not supported 

# Create task
task_lbl = paste0("a=", alphax)
task = TaskSurv$new(id = "veteran", label=task_lbl, backend = veteran, time = time_nm, event =  event_nm)

# Define weights for the observations (example: give each instance equal weight)
weights = rep(1, task$nrow)
# Add weights to the task
task$cbind(data.table(weights = weights))

# Define the weights column role
task$set_col_roles("weights", roles = "weight")


# Define and train the learner
learner = lrn("surv.cv_glmnet")

# set parameters cv.glmnet arguments not defined in task
p =tasj$ncol -1
penalty_factor = rep(1, p)  # Example: applying equal penalties to all predictors

learner$param_set$values = list(
     alpha = alphax,
     nfolds = nfolds,
     penalty.factor = penalty_factor
     )

# Train the learner on the task, including the weights
learner$train(task)

# Access the trained model
model = learner$model
optimum_lambda = model$lambda.min
predictions = learner$predict(task)

# Using the function
t_star = 90  # Example time point
risk_scores = predict_risk_at_time(learner, task, t_star)
print(risk_scores)