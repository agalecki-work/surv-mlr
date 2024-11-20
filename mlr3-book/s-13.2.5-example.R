## 13.2.5 Putting It All Together

library(mlr3extralearners)
library(mlr3verse)
library(mlr3proba)


tsk_grace = tsk("grace")
tsk_grace$filter(sample(tsk_grace$nrow, 500))
msr_txt = c("surv.rcll", "surv.cindex", "surv.dcalib")
measures = msrs(msr_txt)

splits = partition(tsk_grace)  

graph_learner = as_learner(ppl(
  "distrcompositor",
  learner = lrn("surv.glmnet"),
  estimator = "kaplan",
  form = "ph"
))
graph_learner$id = "Coxnet"

prediction_coxnet = graph_learner$train(tsk_grace, split$train)$
  predict(tsk_grace, split$test)
prediction_coxnet
prediction_coxnet$distr[1:3]$survival(77)


learners = c(lrns(c("surv.coxph", "surv.kaplan")), graph_learner)

bmr = benchmark(benchmark_grid(tsk_grace, learners,
  rsmp("cv", folds = 3)))
bmr$aggregate(measures)[, c("learner_id", ..msr_txt)]