# source("surv-networks.R")

# https://mlr3gallery.mlr-org.com/posts/2021-04-20-survival-networks/
rm(list=ls())
library(mlr3)
library(mlr3proba)

## get the `whas` task from mlr3proba
whas <- tsk("whas")

## create our own task from the rats dataset
rats_data <- survival::rats
## convert characters to factors
rats_data$sex <- factor(rats_data$sex, levels = c("f", "m"))
rats <- TaskSurv$new("rats", rats_data, time = "time", event = "status")

## combine in list
tasks <- list(whas, rats)


#-------- Getting and tuning learners
# -- Parameter space
# Dropout fraction tuned over [0, 1]
# Weight decay over [0, 0.5]
# Learning rate over [0, 1]
# Number of nodes in a layer over {1, ... ,32}
# Number of hidden layers over {1, ...,4}

library(paradox)

ufun = function(x, param_set) {
  x$num_nodes = rep(x$nodes, x$k)
  x$nodes = x$k = NULL
  return(x)
}

search_space = ps(
  # p_dbl for numeric valued parameters
  dropout = p_dbl(lower = 0, upper = 1),
  weight_decay = p_dbl(lower = 0, upper = 0.5),
  learning_rate = p_dbl(lower = 0, upper = 1),
  
  # p_int for integer valued parameters
  nodes = p_int(lower = as.integer(1), upper = as.integer(32)),
  k = p_int(lower = 1, upper = 4),
  .extra_trafo = ufun
)


# Autotuner
library(mlr3tuning)
create_autotuner <- function(learner) {
  AutoTuner$new(
   learner = learner,
   search_space = search_space,
   resampling = rsmp("holdout"),
   measure = msr("surv.cindex"),
   terminator = trm("evals", n_evals = 2),
   tuner = tnr("random_search"))
}

## learners are stored in mlr3extralearners
library(mlr3extralearners)

## load learners
learners <- lrns(paste0("surv.", c("coxtime", "deephit", "deepsurv", 
                                   "loghaz", "pchazard")),
                 frac = 0.3, early_stopping = TRUE, epochs = 10,
                 optimizer = "adam"
)
 
# apply our function
learners <- lapply(learners, create_autotuner)

#---- Preprocessing

library(mlr3pipelines)

create_pipeops <- function(learner) {
  po("encode") %>>% po("scale") %>>% po("learner", learner)
}

# apply our function
learners <- lapply(learners, create_pipeops)

#---- Benchmark
## select holdout as the resampling strategy
resampling <- rsmp("cv", folds = 3)

## add KM and CPH
learners <- c(learners, lrns(c("surv.kaplan", "surv.coxph")))
design <- benchmark_grid(tasks, learners, resampling)
bm <- benchmark(design)

## Concordance index and Integrated Graf Score
msrs <- msrs(c("surv.cindex", "surv.graf"))
bm$aggregate(msrs)[, c(3, 4, 7, 8)]

#--- Results
library(mlr3benchmark)

## create mlr3benchmark object
bma <- as.BenchmarkAggr(bm, 
                        measures = msrs(c("surv.cindex", "surv.graf")))

## run global Friedman test
bma$friedman_test()


