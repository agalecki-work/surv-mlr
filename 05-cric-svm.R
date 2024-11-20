### source("05-cric-svm.R")

#Survival support vector machine. Calls survivalsvm::survivalsvm()

rm(list=ls())


# Load libraries
pkgs <- c("mlr3", "mlr3proba", "mlr3learners", "mlr3extralearners", "glmnet", "mlr3pipelines","dplyr", 
    "data.table", "mlr3viz")
lapply(pkgs, require, quietly =TRUE, character.only = TRUE)


#=======  Analysis info

#-- Target vars `time` and `event` defined

t_vars  =   c(time = "TIME_ESRD", event = "ESRD") # named vector

OLx         = paste0("OL", 1:21) # OL1,..., OL21
spline_vars = c("OL1", "OL2")
x_vars      = c("AFIB", "AGE_INTEGER", "BMI", "CHF")  
fac_vars    = c("ACRcat", "CKD", "RACE_CAT_1")
feature_vars = c(x_vars, fac_vars, OLx) 
feature_test = c(x_vars, "OL1", "OL2")

# Info list for TaskSurv
Info = list(
   analysis_id    = paste0("cric_svm: ", t_vars[1]),
   df_complete_nm = c("df_complete", "./results/cric_complete112023.Rdata"), # name, path
   id_col        = "PID",
   target_vars   = t_vars,
   feature_cols  = feature_test,
   # weight_col    = "wght",  # Weight is not allowed for `surv.coxph` learner
   time_horizon  = 7,
   spline_vars   = spline_vars,              # subset of feature_cols
   save_path     = "./results/05-cric-coxph.Rdata"
)
# rm(OLx, spline_vars,t1_vars, t2_vars, t3_vars,t4_vars, x_vars, fac_vars,feature_test, feature_vars)

#-- Unpack Info


# Load `df_complete`
load(file =  Info$df_complete_nm[2], verbose=TRUE)
print(Info_df_complete)
print(var_labels)
print(names(df_complete))

#========== Derive new vars/modify `df_complete`

# `df_complete`: Convert to factors. Create weights, etc !!!
df_complete = within(df_complete,{
   ACRcat   = factor(ACRcat)
   CKD      = factor(CKD)
   CHF      = factor(CHF)
   AFIB     = factor(AFIB)
   RACE_CAT_1 = factor(RACE_CAT_1)
   # wght     = 1.2 # Note `wght` is a new variable and has to be declared in `Info`
})


# Keep `t_vars` specific for this analysis only (skip remaining t_vars)
keep_vars = with(Info, c(id_col, target_vars["time"], target_vars["event"], feature_cols))

setdiff(keep_vars, names(df_complete)) 
setdiff(keep_vars, names(df_complete))
df_complete = df_complete[, keep_vars] 


#-- Admin censoring

apply_admin_censoring <- function(data, time_col, event_col, censor_time = Inf) {
    censor_timex = if (is.infinite(censor_time)) max(time_col) else censor_time 
    setDT(data)  # Ensure the data is a data.table
    data[, (event_col) := fifelse(get(time_col) > censor_time, 0, get(event_col))]
    data[, (time_col)   := pmin(get(time_col), censor_timex)]
    return(data)
}


df_complete = apply_admin_censoring(df_complete, 
                  time_col      = Info$target_vars["time"], 
                  event_col     = Info$target_vars["event"],
                  censor_time   = Info$time_horizon)


#==== Task_surv defined
task0 = as_task_surv(df_complete, id = paste0("task0_", Info$analysis_id), 
                             time = Info$target_vars["time"], event = Info$target_vars["event"])
task0$set_col_roles(Info$id_col, roles = "group")
#if (length(weight_col) ==1) task0$set_col_roles(weight_col, roles = "weight")
task0$set_col_roles(Info$feature_cols, roles = "feature")
task0$head()
# autoplot(task0)

set.seed(4321)
split = partition(task0)


  mylearner1 = lrn("surv.svm", type = "regression" , gamma.mu = 1e-3)
  # sort(names(mylearner))

  # pass the task to the learner via $train()
  mylearner1$train(task0, row_ids = split$train)

  # inspect the trained model
  # mylearner1$model

#===== Prediction on test data using trained learner 
          
  prediction1 = mylearner1$predict(task0, row_ids = split$test)   # prediction on test data        
  #prediction1
  
  pred_types = mylearner1$predict_types
  
  if ("distr" %in% pred_types){
   #--- predict_type = "distr"
 
   prediction1$distr[1:3]$survival(c(3,5))
  } #

#---- predict_type = "crank" stands for continuous ranking. 
#-  In mlr3proba there is one consistent interpretation of crank: 
# lower values represent a lower risk of the event taking place and higher values represent higher risk.
   if ("crank" %in% pred_types) prediction1$crank[1:3]

## ==== MeasureSurv

##as.data.table(mlr_measures)[
##  task_type == "surv", c("key", "predict_type")][1:5]
  
# surv.rcll  RCLL (right-censored logloss) to evaluate the quality of distr predictions
# concordance index to evaluate a model’s discrimination,
# D-Calibration to evaluate a model’s calibration 
#-- prediction1$score(msrs(c("surv.graf", "surv.rcll", "surv.dcalib"))) NaN omitted
prediction1$score(msrs(c("surv.cindex")))






           








 