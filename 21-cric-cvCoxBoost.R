### source("21-cric-cvCoxBoost.R")

rm(list=ls())

# Load libraries
pkgs <- c("mlr3", "mlr3proba", "mlr3learners", "mlr3extralearners", "glmnet", "mlr3pipelines","dplyr", 
    "data.table", "mlr3viz","survival") #
res = lapply(pkgs, require, quietly =TRUE, character.only = TRUE)

source("./R/accept_col_roles.R")

# Load `df_complete`
load(file = "./results/cric_complete112023.Rdata", verbose=TRUE)
# print(Info_df_complete)
# print(var_labels)
nms_df_complete = names(df_complete)

# Source script to add new (or modify existing) variables in `df_complete` (add as many vars as you wish) 
nms_df_complete0 = names(df_complete)
source("./src/cric_add_variables.R")
nms_df_complete = names(df_complete)
vars_added = setdiff(nms_df_complete, nms_df_complete0)
cat("--- Var names added to `df_complete` are stored in `vars_added` vector (n=", length(vars_added), ")\n") 

#=======  Create `Info` list 

#---  Target vars `time` and `status` defined
#-    Note: event variable has two representations (numeric and factor)
t_vars1 = c(time= "eTIME_ESRD", event= c(num= "event_ESRDx", fac = "event_ESRDf"))
t_vars2   = c(time= "TIME_ESRD", event= c(num= "ESRD"))
t_vars    = t_vars1 # Choose one
OLx       = paste0("OL", 1:21) # OL1,..., OL21
x_vars    = c("AFIB", "AGE_INTEGER", "BMI", "CHF")  
fac_vars  = c("ACRcat", "CKD", "RACE_CAT_1")

feature_vars = c(x_vars, fac_vars, OLx) 
feature_test = c(x_vars, fac_vars, "OL1", "OL2")

# Info list 
    special_penalty = "optimCoxBoostPenalty"

opt_step = 153
learner_args = list(maxstepno = opt_step, penalty=special_penalty, criterion = "pscore", unpen.index=c(1,2))

Info = list(
   analysis_lbl  = paste0("cric-cv.coxnet-FG: ", t_vars["time"]),
   data_complete = c("df_complete", "./results/cric_olinknpx_112023_v1.Rdata"), # name, path
   target_cols   = t_vars,
   feature_cols  = feature_test,
   d_cols        = c(id = "PID", weight = "wght", "log_OL1"), # Additonal variables used in the analysis
   time_horizon  = 7,
   partition_ratio = 0.7,
   learner_args   = learner_args,
   splined_vars   = c("OL1") #,
   # pen_factor_modify = c(BMI = 0, CKD. = 0) # Note dot after CKD (expanded factor)
)

# check Info list
names_dcols0 = names(Info$d_cols)
names_dcols  = setdiff(names_dcols0, "")
cat("--- Info check: Names of the elements in Info$d_cols vector (Note: id is obligatory) \n", names_dcols, "\n")

# rm(OLx, spline_vars,t1_vars, t2_vars, t3_vars,t4_vars, x_vars, fac_vars,feature_test, feature_vars)


#---  Extract info from `Info` list and create auxiliary vectors
nm_id = Info$d_cols["id"]
time_nm  = Info$target_cols["time"]      # character string
evnt_num = Info$target_cols["event.num"] # mandatory string
evnt_fac = Info$target_cols["event.fac"] # character string or NA

if (!is.na(evnt_fac)){
 event_fac = df_complete[[evnt_fac]]           # factor needed for F-G
 event_lvls = levels(event_fac)
 event_table = table( df_complete[[evnt_fac]])
 cat("--- Table for event factor: `", evnt_fac, "` \n", event_lvls , "\n", event_table, "\n" )
} else {
 event_lvls = c("0-censored", paste0("1-", evnt_num))
 event_table = table( df_complete[[evnt_num]]) # numeric event
 cat("--- Table for event variable: `", evnt_num, "` \n" , "\n", event_table, "\n" )
}

#====  Keep selected columns in `df_complete`
cat("--- Check `keep_vars`:")
source("./src/02keep_selected_cols.R")

#==== Admin censoring
source("./R/apply_admin_censoring.R")
cat("--- admin censoring at:", Info$time_horizon, "\n")
df_complete = apply_admin_censoring(df_complete, time_col = time_nm, 
                event_col = evnt_num,
                censor_time = Info$time_horizon)
                               
# Note: `df_complete`  Contains both event.num and event.fac variables
##--- Define `pom` object of Graph class

source("./src/create_pomGraph_object.R")
 
cat("--- Event variable has ", length(event_lvls), "levels \n")
if (length(event_lvls) == 3) source("./src/create_task1e_FG.R") else source("./src/create_task1e.R") 
 cat("--- task1e created \n")
 feature_nms0 = names(task1e$data()) # using df_expanded_num
 feature_nms  = setdiff(feature_nms0, c("time_nm", "evnt_num", "fgstart", "fgstop", "fgstatus"))

# Use `feature_nms` and `Info$pen_factor_modify` to create `pen.fac` vector
    #source("./src/create_penfactor.R") 
    #cat("--- Vector `pen.fac` with ", length(pen.fac), " elements created \n")

    set.seed(4321)
    rat = Info$partition_ratio
    split = partition(task1e, ratio = rat)
    cat("--- `task1e` partitioned with ratio ", rat, " \n")
 
 # Define learner
    learner = lrn("surv.cv_coxboost")
    learner_args = c(Info$learner_args) #, list(penalty.factor = pen.fac))
    learner$param_set$values[names(Info$learner_args)] <- Info$learner_args
    cat("--- Learner defined \n")
    
 # pass the task to the learner via $train()
    learner$train(task1e, split$train)
    prediction = learner$predict(task1e, row_ids = split$test)   # prediction on test data 
    cat("--- prediction on test data \n")

# inspect the trained model
# coef(learner$model$model)

 
#--- predict_type = "distr"

prediction$distr[1:3]$survival(c(1, 3, 5, 7)) # prob surviving 5 years for the first 3 subjects in test data

#---- predict_type = "crank" stands for continuous ranking. 
#-  In mlr3proba there is one consistent interpretation of crank: 
# lower values represent a lower risk of the event taking place and higher values represent higher risk.
prediction$crank[1:3]

## ==== MeasureSurv

as.data.table(mlr_measures)[
  task_type == "surv", c("key", "predict_type")][1:5]
  
# surv.rcll  RCLL (right-censored logloss) to evaluate the quality of distr predictions
# concordance index to evaluate a model’s discrimination,
# D-Calibration to evaluate a model’s calibration 
prediction$score(msrs(c("surv.graf", "surv.rcll", "surv.cindex", "surv.dcalib")))


# Plots
task_plot = task1$clone()

autoplot(task_plot, type = "target") # Survival curve
task_plot$select("BMI")
autoplot(task_plot, type = "duo")
# autoplot(mylearner, type = "prediction", task1e) # not suppoted

 