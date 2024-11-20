# source("021-CoxBoost.R")

rm(list=ls())

require(mlr3)
require(mlr3proba)
require(mlr3learners)
require(mlr3extralearners)
require(mlr3tuning)
require(mlr3verse)
library(CoxBoost)
set.seed(1324)
n <- 200; p <- 100
bbeta <- c(rep(1,10),rep(0,p-10))
x <- matrix(rnorm(n*p),n,p)
real.time <- -(log(runif(n)))/(10*exp(drop(x %*% bbeta)))
cens.time <- rexp(n,rate=1/10)
status <- ifelse(real.time <= cens.time,1,0)
obs.time <- ifelse(real.time <= cens.time,real.time,cens.time)

#  determine penalty parameter
# 

optim.res <- optimCoxBoostPenalty(time=obs.time,status=status,x=x,
                                  trace=TRUE, start.penalty=500)
                                  
                                 
opt_step = optim.res$cv.res$optimal.step
opt_pen  = optim.res$penalty

#   Fit a Cox proportional hazards model by CoxBoost
#   ... with covariates 1 and 2 being mandatory


cbfit.mand <- CoxBoost(time=obs.time,status=status,x=x,unpen.index=c(1,2), stepno=opt_step,penalty=opt_pen)

summary(cbfit.mand)

df = data.frame(time=obs.time, status=status, x) 

# Create a survival task
task <- TaskSurv$new("survival", backend = df, 
                      time = "time", event ="status",
                      type = "right", # default
                      label = "???")
                      
special_penalty = "optimCoxBoostPenalty"

learner_args = list(maxstepno = opt_step, penalty=special_penalty, criterion = "pscore", unpen.index=c(1,2))
learner = lrn("surv.cv_coxboost")
learner$param_set$values[names(learner_args)] = learner_args
splits = partition(task)
learner$train(task, splits$train)
tt = learner$model
sum(coef(tt) !=0)


pred = learner$predict(task, splits$test)

