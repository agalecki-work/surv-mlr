rm(list=ls())


# Load libraries
#pkgs <- c("mlr3", "mlr3proba", "mlr3learners", "mlr3extralearners", "glmnet", "mlr3pipelines","dplyr", 
#    "data.table", "mlr3viz") #
#tt = lapply(pkgs, require, quietly =TRUE, character.only = TRUE)

library(mlr3verse)
library(data.table)
library(survival)

mgus2 = survival::mgus2
mgus2 <- mgus2[mgus2$id <= 100,]
dim(mgus2)

etime <- with(mgus2, ifelse(pstat==0, futime, ptime))
event <- with(mgus2, ifelse(pstat==0, 2*death, 1))
event <- factor(event, 0:2, labels=c("censor", "pcm", "death"))
mgus2$etime = etime
mgus2$event = event
mgus2$sex   = NULL
names(mgus2)      
             
# FG model for PCM
pdata <- finegray(Surv(etime, event) ~ ., data=mgus2)
names(pdata)
pdata = within(pdata, ptime <- pstat <- death <- NULL)
pdata = pdata[complete.cases(pdata),]

#==== `task_FG`
task_FG = TaskSurv$new(id = "F-G", backend = pdata, 
                             time = "fgstart", time2 = "fgstop",  event = "fgstatus",
                             type = "counting")
task_FG$set_col_roles("id", roles = "group")
task_FG$set_col_roles("fgwt", roles = "weight")
task_FG                            

learner = lrn("surv.cv_glmnet", alpha = 0.5) #
# pass the task to the learner via $train()

learner$train(task_FG)

#--- https://stackoverflow.com/questions/32952164/survfit-coxph-predicting-survival-using-newdata-and-id-option
mdl = learner$model
names(mdl)
cv_fit = mdl$model
best_lambda <- cv_fit$lambda.min

new_data = 

x = mdl$x
y = mdl$y
w = mdl$weights
head(y)
# str(cvfit)
newx = x[1:5,]
idx  = pdata$id[1:5]
srvfit =survival::survfit(cvfit, x = x, y = y, weights = w, newx = newx) #, id=idx)

# Use srvfit for new data
newdata = mgus2[1:5]
summary(srvfi,t

prediction = learner$predict(task_FG)   # prediction on all data        
pred_df = as.data.table(prediction) 

#--- predict_type = "distr"

prediction$distr[1:3]$survival(c(100, 150, 300)) # prob surviving 5 years for the first 3 subjects in  pdata

#