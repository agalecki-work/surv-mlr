### source("20-cric-cv.coxnet-withFG.R")

rm(list=ls())
#source("./R/custom_fg_metric.R")

# Load libraries
pkgs <- c("mlr3", "mlr3proba", "mlr3learners", "mlr3extralearners", "glmnet", "mlr3pipelines","dplyr", 
    "data.table", "mlr3viz","survival", "dplyr") #
res = lapply(pkgs, require, quietly =TRUE, character.only = TRUE)
source("./R/accept_col_roles.R")

cat("--- Packages loaded \n")

prj_path = "./CRIC_prj"   ## !!!

fpath = paste0(prj_path, "/prj_Info.inc")
source(fpath)
cat("=== source `prj_Info.inc` list created \n")

dfin_info = prj_Info$dfin_info


####  Load list with the results from 02*.R script, including `df_complete`
load(file = dfin_info["fpath"] , verbose=TRUE)
assign("df_complete", eval(parse(text= dfin_info["name"]))) 
cat("--- Objects loaded \n")

# print(Info_df_complete)
# print(var_labels)
nms_df_complete = names(df_complete)

# Source script to add new (or modify existing) variables in `df_complete` (add as many vars as you wish) 
nms_df_complete0 = names(df_complete)
add_vars_inc = prj_Info$add_vars_inc
source(add_vars_inc)
nms_df_complete = names(df_complete)
vars_added = setdiff(nms_df_complete, nms_df_complete0)
cat("--- Var names added to `df_complete` are stored in `vars_added` vector (n=", length(vars_added), ")\n") 

#=======  Create `Info` list 

#---  Target vars `time` and `status` defined
#-    Note: event variable has two representations (numeric and factor)
t_vars1 = c(time= "eTIME_ESRD", event= c(num= "event_ESRDx", fac = "event_ESRDf"))
t_vars2   = c(time= "TIME_ESRD", event= c(num= "ESRD"))
t_vars    = t_vars2 # Choose one
OLx       = paste0("OL", 1:21) # OL1,..., OL21
x_vars    = c("AFIB", "AGE_INTEGER", "BMI", "CHF")  
fac_vars  = c("ACRcat", "CKD", "RACE_CAT_1")

feature_vars = c(x_vars, fac_vars, OLx) 
feature_test = c(x_vars, fac_vars, "OL1", "OL2")

# Info list 


nfolds = 5
glmnet_args = list(alpha = 0.5, s="lambda.min", nfolds = nfolds)

Info = list(
   analysis_lbl  = t_vars["time"], 
   target_cols   = t_vars,
   feature_cols  = feature_test,
   d_cols        = c(id = "PID",            # id variable (mandatory) role "group" will be assigned
                     weight = "wght",
                     "log_OL1"),            # Declare additonal variables used in the analysis
   time_horizon  = 10,
   partition_ratio = 0.8, # split
   learner_args   = glmnet_args,
   subset         = "CKD == 'CKD3b'",
   splined_vars   = c("OL1"),
   pen_factor_modify = c(BMI = 0, CKD. = 0), # Note dot after CKD (expanded factor),
   seed           = c(foldid = 1324, split = 7943) 
)

# check Info list
names_dcols0 = names(Info$d_cols)
names_dcols  = setdiff(names_dcols0, "")
cat("--- Info check: Names of the elements in Info$d_cols vector (Note: id is mandatory) \n", names_dcols, "\n")

# rm(OLx, spline_vars,t1_vars, t2_vars, t3_vars,t4_vars, x_vars, fac_vars,feature_test, feature_vars)
cat("=== `df_complete` before subset:", Info$subset, "nrows =",  nrow(df_complete),"\n")
esubset = parse(text= Info$subset)
df_complete = subset(df_complete, subset= eval(esubset))
cat("--- `df_complete` after subset. nrows =",  nrow(df_complete),"\n")


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
 cat("--- Table for event variable: `", evnt_num, "` \n" , event_table, "\n" )
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
    source("./src/create_penfactor.R") 
    cat("--- Vector `pen.fac` with ", length(pen.fac), " elements created \n")
    cat("--- `task1e` partitioned with ratio ", rat, " \n")
 
 # Define learner
    learner = lrn("surv.cv_glmnet")
    learner_args = c(Info$learner_args, list(penalty.factor = pen.fac, foldid=foldid))
    learner$param_set$values[names(Info$learner_args)] <- Info$learner_args
    cat("--- Learner defined \n")
    
 # pass the task to the learner via $train()
 if (length(event_lvls) == 3) learner$train(task1e_FG, row_ids = train_indices_fg) else learner$train(task1e, split$train) 
 
 # Get predictions for your Fine-Gray task
 # prediction_FG = learner$predict(task1e_FG, row_ids = test_indices_fg)

 # score = prediction_FG$score(measure_fg_concordance)
 # print(score)
  
  
    prediction = learner$predict(task1e, row_ids = split$test)   # prediction on test data 
    cat("--- prediction on test data \n")

# inspect the trained model
# coef(learner$model$model)

 
#--- predict_type = "distr"

prediction$distr[1:3]$survival(c(1, 3, 5, 7)) # prob surviving 5 years for the first 3 subjects in test data
cat("--- prediction survival \n")


#---- predict_type = "crank" stands for continuous ranking. 
#-  In mlr3proba there is one consistent interpretation of crank: 
# lower values represent a lower risk of the event taking place and higher values represent higher risk.
prediction$crank[1:3]
cat("--- prediction crank \n")

## ==== MeasureSurv

as.data.table(mlr_measures)[
  task_type == "surv", c("key", "predict_type")][1:5]
  
# surv.rcll  RCLL (right-censored logloss) to evaluate the quality of distr predictions
# concordance index to evaluate a model’s discrimination,
# D-Calibration to evaluate a model’s calibration 
if (length(event_lvls) == 2) prediction$score(msrs(c("surv.graf", "surv.rcll", "surv.cindex", "surv.dcalib")))


if (length(event_lvls) == 2){
  save_objects = c("Info", "learner", "prediction")
  #fpath = paste0(prj_path,"/lvl"),
  #list= save_objects, 
  
  

}


# Plots
task_plot = task1$clone()

if (length(event_lvls) == 2) autoplot(task_plot, type = "target") # Survival curve
task_plot$select("BMI")
autoplot(task_plot, type = "duo")
# autoplot(mylearner, type = "prediction", task1e) # not supported

 