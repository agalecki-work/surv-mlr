## 13.2 Survival Analysis

## 13.2.1 TaskSurv

library(mlr3verse)
library(mlr3proba)
library(survival)

tsk_rats = as_task_surv(survival::rats, time = "time",
  event = "status", type = "right", id = "rats")
tsk_rats$head()
autoplot(tsk_rats)

as.data.table(mlr_tasks)[task_type == "surv"]

## 13.2.2 LearnerSurv, PredictionSurv and Predict Types
set.seed(349)
tsk_rats = tsk("rats")
split = partition(tsk_rats)
prediction_cph = lrn("surv.coxph")$train(tsk_rats, split$train)$
  predict(tsk_rats, split$test)
prediction_cph

## predict_type = “response” (the least common)
# Type= regression is used
# Predicted time is compared with true data 
library(mlr3extralearners)
prediction_svm = lrn("surv.svm", type = "regression", gamma.mu = 1e-3)$
  train(tsk_rats, split$train)$predict(tsk_rats, split$test)
data.frame(pred = prediction_svm$response[1:3],
  truth = prediction_svm$truth[1:3]) 

## predict_type = "distr"
# three $distr predictions from our example and calculate the probability of survival at t=77
prediction_cph$distr[1:3]$survival(77)

##--- predict_type = "lp"
#     lp is a proxy for a relative risk/continuous ranking prediction (see below)

#---- predict_type = "crank" stands for continuous ranking. 
#-  In mlr3proba there is one consistent interpretation of crank: 
# lower values represent a lower risk of the event taking place and higher values represent higher risk.
prediction_cph$crank[1:3]


##  13.2.3 MeasureSurv

as.data.table(mlr_measures)[
  task_type == "surv", c("key", "predict_type")][1:5]
  
# surv.rcll  RCLL (right-censored logloss) to evaluate the quality of distr predictions
# concordance index to evaluate a model’s discrimination,
# D-Calibration to evaluate a model’s calibration 
prediction_cph$score(msrs(c("surv.rcll", "surv.cindex", "surv.dcalib")))

## 13.2.4 Composition

## 13.2.4.1 Internal Composition

## 13.2.4.2 Explicit Composition and Pipelines

library(mlr3verse)
library(mlr3extralearners)

tsk_rats = tsk("rats")$select(c("litter", "rx"))
split = partition(tsk_rats)

learner = lrn("surv.glmnet")

# no distr output
learner$train(tsk_rats, split$train)$predict(tsk_rats, split$test)


graph_learner = as_learner(ppl(
  "distrcompositor",
  learner = learner,
  estimator = "kaplan",
  form = "ph"
))

# now with distr
graph_learner$train(tsk_rats, split$train)$predict(tsk_rats, split$test)

## 13.2.5 Putting It All Together

library(mlr3extralearners)

tsk_grace = tsk("grace")
tsk_grace$filter(sample(tsk_grace$nrow, 500))
msr_txt = c("surv.rcll", "surv.cindex", "surv.dcalib")
measures = msrs(msr_txt)

graph_learner = as_learner(ppl(
  "distrcompositor",
  learner = lrn("surv.glmnet"),
  estimator = "kaplan",
  form = "ph"
))
graph_learner$id = "Coxnet"
learners = c(lrns(c("surv.coxph", "surv.kaplan")), graph_learner)

bmr = benchmark(benchmark_grid(tsk_grace, learners,
  rsmp("cv", folds = 3)))
bmr$aggregate(measures)[, c("learner_id", ..msr_txt)]


