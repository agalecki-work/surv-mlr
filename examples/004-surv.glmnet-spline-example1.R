# source("004-surv.glmnet-spline-example.R")

rm(list=ls())

# Required libraries
 library(mlr3)
 library(mlr3proba)
 library(mlr3learners)
 library(mlr3extralearners)
 library(mlr3tuning)
 library(mlr3pipelines)
 library(survival)
 library(splines)
 library(data.table)
 library(glmnet)
 
# Simulate data
 set.seed(123)
     n = 100
     p = 3
     X = matrix(rnorm(n * p), nrow = n, ncol = p)
     time = rexp(n, rate = 1)
     status = sample(0:1, n, replace = TRUE)
     df = as.data.frame(X)
     df$time = time
     df$status = status

# Define a survival task

task  =  TaskSurv$new("survival_task", backend = df, time = "time", event = "status")

  learner = lrn("surv.glmnet")
  learner$param_set$set_values(.values = list(
        alpha  = to_tune(0, 1),
        lambda = to_tune(p_dbl(0.00001, 1, logscale = TRUE))
      ))
      
      
  apply_splines = function(x) {
       as.data.table(splines::ns(x, df = 3))
      }
 
# Define  grlrn for applying splines transformation
  grlrn0 = 
       po("colapply", id = "spline_all", 
          applicator = apply_splines, 
          affect_columns = selector_type("numeric")) %>>%
       po("learner", learner)
  grlrn = GraphLearner$new(grlrn0)

# Resampling strategy for tuning
     resampling = rsmp("cv", folds = 5)
 
# Performance measure for survival analysis
     measure  =  msr("surv.cindex")
     
# Create the tuner
     tuner  = tnr("grid_search", resolution = 5)
     
# Define the AutoTuner
   at = auto_tuner(
       learner = grlrn,
       resampling = resampling,
       measure = measure, 
       ### search_space = search_space, # omitted
       terminator = trm("evals", n_evals = 50),
       tuner = tuner
      )
      
# Simple train/test split
   part = partition(task)
   at$train(task, row_ids = part$train)
  
  at$model
  at$tuning_result
    


