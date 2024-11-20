# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8428574/
# mlr3proba: an R package for machine learning in survival analysis


library(mlr3)
library(mlr3proba)
library(mlr3learners)
library(mlr3pipelines)

rm(list=ls())

kaplan = lrn( "surv.kaplan")
cox    = lrn( "surv.coxph")
xgb    = ppl( "distrcompositor",
           learner = lrn("surv.xgboost.cox"),
           estimator = "kaplan" , 
           form      = "ph")

learners = list(cox, kaplan, xgb)

task = TaskSurv$new(id = "rats" ,
           backend = survival::rats[ , 1:4],
           time = "time" , event = "status")
resample = rsmp("cv" , folds = 3)
design   = benchmark_grid(task, learners, resample)
bm = benchmark(design)

bm$aggregate(msr("surv.intlogloss"))

