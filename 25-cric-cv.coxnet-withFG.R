### source("25-cric-cv.coxnet-withFG.R")

rm(list=ls())
prj_path = "./CRIC_prj"   ## !!!


# Load libraries
pkgs <- c("mlr3", "mlr3proba", "mlr3learners", "mlr3extralearners", "glmnet", "mlr3pipelines","dplyr", 
    "data.table", "mlr3viz","survival", "dplyr") #
res = lapply(pkgs, require, quietly =TRUE, character.only = TRUE)
source("./R/accept_col_roles.R")

cat("--- Packages loaded \n")

fpath = paste0(prj_path, "/prj_Info.inc")
source(fpath) # 
names(prj_Info) # prj_name, Rdata_name, add_vars_name
project_name = prj_Info[["prj_name"]]
anl_name  = prj_Info[["analysis"]]
cat("=== source `prj_Info.inc`.`prj_Info` list created \n")


####  Load list with the results from 02*.R script, including `df_complete`
tmp = prj_Info$complete_nms["Rdata"]
fpath = paste0(prj_path, "/data/", tmp)  
load(file = fpath , verbose=TRUE)
tmpx = prj_Info$complete_nms["df"]
assign("df_complete", eval(parse(text= tmpx ))) 
cat("--- Objects loaded \n")

# print(Info_df_complete)
# print(var_labels)
nms_df_complete = names(df_complete)

# Source script to add new (or modify existing) variables in `df_complete` (add as many vars as you wish) 
nms_df_complete0 = names(df_complete)
add_vars_inc = prj_Info$add_vars_name
add_vars_path = paste0(prj_path,"/", add_vars_inc)
source(add_vars_path)
nms_df_complete = names(df_complete)
vars_added = setdiff(nms_df_complete, nms_df_complete0)
cat("--- Var names added to `df_complete` are stored in `vars_added` vector (n=", length(vars_added), ")\n") 

#=======  Create `Info` list 
fpath = paste0(prj_path,"/", anl_name, "/InfoList.inc")
source(fpath)
cat("--- Info list on analysis in subfolder: ", anl_name, " created \n") 

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
 feature_nms  = setdiff(feature_nms0, c(time_nm, evnt_num, evnt_fac,"fgstart", "fgstop", "fgstatus"))

#==== Process learners  ============
learner_info = Info$learner_info
learner_args = get(learner_info["args"])

#---- `surv.cv_glmnet` learner 
if (Info$learner_info["type"] == "surv.cv_glmnet"){

 # Use `feature_nms` and `Info$pen_factor_modify` to create `pen.fac` vector
     source("./src/create_penfactor.R") 
     cat("--- Vector `pen.fac` with ", length(pen.fac), " elements created \n")
     cat("--- `task1e` partitioned with ratio ", rat, " \n")
 
 alphas_nms = names(Info$alphas)
 for (ai in seq_along(Info$alphas)){
 # Define learner
    
    ax= Info$alphas[ai]
    anm = alphas_nms[ai]
    # cat("--- Alpha =", ax, "processed \n") 
    learner = lrn("surv.cv_glmnet")
    learner_args_all = c(learner_args, list(alpha = ax, penalty.factor = pen.fac, foldid=foldid))
    learner$param_set$values[names(learner_args_all)] <- learner_args_all
    cat("--- Learner defined (type =`surv.cv_glmnet`, alpha=", ax, ") \n")
    
 # pass the task to the learner via $train()
 if (length(event_lvls) == 3) learner$train(task1e_FG, split$train) else learner$train(task1e, split$train) 
 
 # Get predictions for your Fine-Gray task
 # prediction_FG = learner$predict(task1e_FG, row_ids = test_indices_fg)

 # score = prediction_FG$score(measure_fg_concordance)
 # print(score)
  
  
    prediction = learner$predict(task1e, row_ids = split$test)   # prediction on test data 
   #  cat("--- prediction on test data \n")

# inspect the trained model
# coef(learner$model$model)
 
#--- predict_type = "distr"

prediction$distr[1:3]$survival(c(1, 3, 5, 7)) # prob surviving 5 years for the first 3 subjects in test data
# cat("--- prediction survival \n")


#---- predict_type = "crank" stands for continuous ranking. 
#-  In mlr3proba there is one consistent interpretation of crank: 
# lower values represent a lower risk of the event taking place and higher values represent higher risk.
prediction$crank[1:3]
# cat("--- prediction crank \n")

## ==== MeasureSurv

as.data.table(mlr_measures)[
  task_type == "surv", c("key", "predict_type")][1:5]
  
# surv.rcll  RCLL (right-censored logloss) to evaluate the quality of distr predictions
# concordance index to evaluate a model’s discrimination,
# D-Calibration to evaluate a model’s calibration 
 if (length(event_lvls) == 2) prediction$score(msrs(c("surv.graf", "surv.rcll", "surv.cindex", "surv.dcalib")))

# Save results
  tvarsx = Info$target_cols
  tvarsx_id = tvarsx["id"]
  tvarsx_tm = tvarsx["time"]

if (length(event_lvls) == 2){
  save_objects = c("Info", "learner", "prediction")
} else {
  cv_glmnet_fit = learner$model$model
  save_objects = c("Info", "cv_glmnet_fit")
}
  
  fpath = paste0(prj_path,"/", anl_name, "/", tvarsx_id , tvarsx_tm,  "_",  anm, ".Rdata")
  #list= save_objects, 
  save(list =save_objects, file =fpath)
  

} # for ax
} # Info$learner_type == "surv.cv_glmnet"

# Other learners
if (length(event_lvls) == 2){
 learners_info = Info$learners_info
 learner_nms = names(learners_info)

 for (i in seq_along(learners_info)){
   lrn_type = learner_nms[i]
   lrn_name = sub("surv.", "",  lrn_type) 
   learner = lrn(lrn_type)
   learner_args = get(learners_info[i])
   learner$param_set$values[names(learner_args)] <- learner_args
   learner$train(task1e, split$train)
   prediction = learner$predict(task1e, row_ids = split$test)
   cat("--- ", lrn_type, "processed ... \n") 
   save_objects = c("Info", "learner", "prediction")
   fpath = paste0(prj_path,"/", anl_name, "/", tvarsx_id , tvarsx_tm,  "_", lrn_name, ".Rdata")
   save(list =save_objects, file =fpath)


} # 
} # if length(event_lvls) == 2

# Plots
# task_plot = task1$clone()

# if (length(event_lvls) == 2) autoplot(task_plot, type = "target") # Survival curve
# task_plot$select("BMI")
#autoplot(task_plot, type = "duo")
# autoplot(mylearner, type = "prediction", task1e) # not supported

 