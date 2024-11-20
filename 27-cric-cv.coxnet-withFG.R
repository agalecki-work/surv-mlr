### source("27-cric-cv.coxnet-withFG.R")

rm(list=ls())

BATmode = TRUE # !!!

if (BATmode){ # Multiple t_vars will be processed in a batch mode
  args <- commandArgs(trailingOnly = TRUE)
  argsx = args[1]  # Ex: {%prj_name%:%anl_name%:%scriptBaseName%} CRIC_prj:test:27-cric-cv.coxnet-withFG
  t_varsid  = args[2]
  cat("====== Script executed in batch mode for `t_varsid=`", t_varsid, "` \n")

} else {
  argsx  ="CRIC_prj:test:27-cric-cv.coxnet-withFG"
  t_varsid = "03" # Ex. 02, 03, 31
  cat("====== Script executed from R console for `t_varsid=`", t_varsid, "` \n")
}
   csplit =  unlist(strsplit(argsx,":", fixed=TRUE))
   prj_name        = csplit[1] # Ex. CRIC_prj
   anl_name        = csplit[2] # Ex. test
   scriptBaseName  = csplit[3] # Ex. 27-cric-cv.coxnet-withFG
   prj_path        = paste0("./",prj_name) 

timeStamp0 = Sys.time()
cat("Time stamp init. t_varsid: ", t_varsid, " \n")
print(as.POSIXlt(timeStamp0))

# Load libraries
pkgs <- c("mlr3", "mlr3proba", "mlr3learners", "mlr3extralearners", "glmnet", "mlr3pipelines","dplyr", 
    "data.table", "mlr3viz","survival", "dplyr","openxlsx", "utilsag","tidyr") #
res = lapply(pkgs, require, quietly =TRUE, character.only = TRUE, warn.conflicts =TRUE )
source("./R/accept_col_roles.R")

cat("--- Packages loaded \n")

fpath = paste0(prj_path, "/prj_Info.inc")
source(fpath) # 
names(prj_Info) # prj_name, Rdata_name, add_vars_name
#??? project_name = prj_Info[["prj_name"]]
#--- anl_name  = prj_Info[["analysis"]]
cat("=== source `prj_Info.inc`.`prj_Info` list created \n")


####  Load list with the results from 02*.R script, including `df_complete`
tmp = prj_Info$complete_nms["Rdata"]
fpath = paste0(prj_path, "/data/", tmp)  
loaded_objects = load(file = fpath)
tmpx = prj_Info$complete_nms["df"]
assign("df_complete", eval(parse(text= tmpx ))) 
cat("--- Objects (including `df_complete`) loaded: ", loaded_objects, "\n")

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
dcol_nms =names(Info$d_cols)
dcol_nms =Info$d_cols[!dcol_nms == ""]
cat("Info$d_col variables: ", dcol_nms, "\n")
time_nm  = Info$target_cols["time"]      # character string
evnt_num = Info$target_cols["event.num"] # mandatory string
evnt_fac = Info$target_cols["event.fac"] # character string or NA
cat("-- Time var nms = ", time_nm, ", status =", evnt_num, ", id =",  Info$target_cols["id"], "\n")
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
max_obstime = max(df_complete[[time_nm]])
cat("--- admin censoring at:", Info$time_horizon, "( maximum observed time=", round(max_obstime, 2) , ") \n")
df_complete = apply_admin_censoring(df_complete, time_col = time_nm, 
                event_col = evnt_num,
                censor_time = Info$time_horizon)
                               
# Note: `df_complete`  Contains both event.num and event.fac variables
##--- Define `pom` object of Graph class

source("./src/create_pomGraph_object.R")
 
cat("--- Event variable has ", length(event_lvls), "levels \n")

CCH = if ("subcohort" %in% names(dcol_nms)) TRUE else FALSE
if (CCH) source("./src/create_task1_cch.R") else source("./src/create_task1.R")
cat("--- task1 created. CCH is ", CCH, "\n")

source("./src/create_task1e.R")
cat("--- task1e created \n")

if (length(event_lvls) == 3) source("./src/create_task1e_FG.R")

 cat("--- task1e created nevent levels: ", length(event_lvls),  "\n")
 feature_nms0 = names(task1e$data()) # using df_expanded_num
 feature_nms  = setdiff(feature_nms0, c(time_nm, evnt_num, evnt_fac,"fgstart", "fgstop", "fgstatus"))

#==== Process learners  ============
cvglmnet_info = Info$cvglmnet_info
cvglmnet_args = get(cvglmnet_info["args"])

#---- `surv.cv_glmnet` learner 
if (Info$cvglmnet_info["type"] == "surv.cv_glmnet"){

 # Use `feature_nms` and `Info$pen_factor_modify` to create `pen.fac` vector
     source("./src/create_penfactor.R") 
     cat("--- Vector `pen.fac` with ", length(pen.fac), " elements created \n")
     cat("--- `task1e` partitioned with ratio ", rat, " \n")
 
 alphas_nms = names(Info$alphas)
 surv.cva_glmnet_learners= vector(mode="list", length = length(alphas_nms))
 names(surv.cva_glmnet_learners) = alphas_nms 
 
 surv.cva_glmnet_prediction= vector(mode="list", length = length(alphas_nms))
 names(surv.cva_glmnet_prediction) = alphas_nms 
 
 cva_glmnet_fits = vector(mode="list", length = length(alphas_nms))
 names(cva_glmnet_fits) = alphas_nms
 
  if (length(event_lvls) == 2) pred1_scores = vector(mode="list", length = length(alphas_nms))

 for (ai in seq_along(Info$alphas)){
 # Define learner
    
    ax= Info$alphas[ai]
    anm = alphas_nms[ai]
    # cat("--- Alpha =", ax, "processed \n") 
    learner = lrn("surv.cv_glmnet")
    learner_args_all = c(cvglmnet_args, list(alpha = ax, penalty.factor = pen.fac, foldid=foldid))
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
 if (length(event_lvls) == 2){
   score_ai = prediction$score(msrs(c("surv.graf", "surv.rcll", "surv.cindex", "surv.dcalib")))
}

# Save results for learner `surv.cv_glmnet`
  tvarsx = Info$target_cols
  tvarsx_id = tvarsx["id"]
  tvarsx_tm = tvarsx["time"]
  
  tmp = learner_args_all$foldid 
  if (length(tmp) > 0) learner_args_all$foldid = paste0(":vector with ", length(tmp), " elements") 
  Info$cv_glmnet_args = learner_args_all

   if (length(event_lvls) == 2){
      save_objects0 = c("surv.cva_glmnet_learners", "surv.cva_glmnet_prediction")
      surv.cva_glmnet_learners[[ai]] = learner
      surv.cva_glmnet_prediction[[ai]] = prediction
      pred1_scores[[ai]] = score_ai
     } else {
      cva_glmnet_fits[[ai]] = learner$model$model
      save_objects0 = c("cva_glmnet_fits")
    }
#--- Cleanup

rm(learner, prediction)
} # for alphas

#--- Results for all alphas
  if (length(event_lvls) == 2){    
     cv_fits = lapply(alphas_nms,  FUN= function(a){
      x=surv.cva_glmnet_learners[[a]]
      x$model$model})
      #--- pred_scoreAll
      pred1_scoreList = lapply(pred1_scores, FUN = function(x) as_tibble(t(x)))
      names(pred1_scoreList) = alphas_nms
      pred_scoreAll = bind_rows( pred1_scoreList, .id = "alpha_lbl")
      pred_scoreAll["learner_lbl"] = "surv.cv_glmnet"

      
    } else cv_fits = cva_glmnet_fits
  
  #--- glanceAll
     glanceList =lapply(cv_fits, myglance)
     names(glanceList) = alphas_nms
     glanceAll =bind_rows(glanceList, .id = "alpha_lbl")
     
     glance_melt = glanceAll %>% select(alpha_lbl, index_min, index_1se) %>%
       gather(index, step,  -alpha_lbl)
       
  #--- tidyAll
     tidyList = lapply(cv_fits, mytidy)
     names(tidyList) = alphas_nms
     tidyAll0 = bind_rows(tidyList, .id = "alpha_lbl") 
     
     tidyAll = tidyAll0 %>% left_join(glance_melt, by= c("alpha_lbl","step"))  %>%
               mutate( index = case_when(
                 is.na(index) ~ "",
                 .default = as.character(index)
                 ))
  
      
  
  #--- coefAll  Beta coefficients
     coef_nms = rownames(coef(cv_fits[[1]]))
     coefList = lapply(cv_fits, FUN= function(x){
        vecx = as.matrix(coef(x))
        dim(vecx) =NULL
        vecx
        })
      coefAll = cbind(coef_nms, bind_cols(coefList))
      names(coefAll) = c("coef", alphas_nms)
    #  coef_alldot <- coefAll %>%  mutate(across(where(is.numeric), ~ round(.,4))) %>%
    #  mutate(across(everything(), ~ ifelse(. == 0, ".", as.character(.))))
  
      
  save_objects = c("data1e", "Info", save_objects0)  
  fpath0 = paste0(prj_path,"/", anl_name, "/", tvarsx_id , tvarsx_tm)
  fpath = paste0(fpath0, "_cva_glmnet.Rdata") 
  save(list =save_objects, file =fpath)
  
  #-- xlsx
  fpath1 =paste0(prj_path,"/", anl_name, "/_prj_info.xlsx")
  wb = loadWorkbook(fpath1)
  addWorksheet(wb, "glanceAll")
  writeData(wb, "glanceAll", glanceAll)
  addWorksheet(wb, "tidyAll")
  writeData(wb, "tidyAll", tidyAll)
  addWorksheet(wb, "coefAll")
  writeData(wb, "coefAll", coefAll)
  fpathx =paste0(fpath0, "_cva_glmnet.xlsx")
} # Info$learner_type == "surv.cv_glmnet"



# Other learners (competing risks not addressed)
if (length(event_lvls) == 2 && !is.null(Info$learners_info)){
 learners_info = Info$learners_info # c(surv.ctree = "ctree_args",  surv.cv_coxboost = "cv_coxboost_args")
 learner_nms = names(learners_info)

 surv_learners= vector(mode="list", length = length(learner_nms))
 names(surv_learners) = learner_nms 
 
 surv_prediction= vector(mode="list", length = length(learner_nms))
 names(surv_prediction) = learner_nms
 
 pred2_scores = vector(mode="list", length = length(learner_nms))
 names(pred2_scores) = learner_nms


 for (i in seq_along(learners_info)){
   lrn_type = learner_nms[i] # Ex. "surv.ctree"
   lrn_name = sub("surv.", "",  lrn_type) # "ctree"
   learner = lrn(lrn_type)
   learner_args = get(learners_info[i])
   learner$param_set$values[names(learner_args)] <- learner_args
   learner$train(task1e, split$train)
   prediction = learner$predict(task1e, row_ids = split$test)
   score_i    = prediction$score(msrs(c("surv.graf", "surv.rcll", "surv.cindex", "surv.dcalib")))
   save_objects0 = c("surv_learners", "surv_prediction")
   surv_learners[[i]] = learner
   surv_prediction[[i]] = prediction
   pred2_scores[[i]] = score_i
   cat("--- ", lrn_type, "processed ... \n") 
 } #  for i
 

     pred2_scoreList = lapply(pred2_scores, FUN = function(x) as_tibble(t(x)))
     names(pred2_scoreList) = learner_nms
     pred2_scoreAll = bind_rows( pred2_scoreList, .id = "learner_lbl")
     pred2_scoreAll["alpha_lbl"] = ""
     pred_scoreAll = rbind(pred_scoreAll, pred2_scoreAll) 
    
    save_objects = c("Info",save_objects0)
     fpath = paste0(prj_path,"/", anl_name, "/", tvarsx_id , tvarsx_tm,  "_varia.Rdata")
     save(list =save_objects, file =fpath)
 
     pred_scoreAll =  pred_scoreAll %>% relocate(learner_lbl, .before = alpha_lbl)
     addWorksheet(wb, "pred_scoreAll")
     writeData(wb, "pred_scoreAll", pred_scoreAll)

} # if length(event_lvls) == 2


  
   
   fpath0 = paste0(prj_path,"/", anl_name, "/", tvarsx_id , tvarsx_tm)
   fpathx =paste0(fpath0, "_summary.xlsx")
   saveWorkbook(wb, fpathx, overwrite = TRUE)

timeStamp1 = Sys.time()
cat("Time stamp. t_varsid: ", t_varsid, " \n")

print(as.POSIXlt(timeStamp1))

fpathx = paste0("./", scriptBaseName, "x.log")  
fpath_map = paste0("./",prj_name, "/", anl_name, "/_map.log")
tdelta = timeStamp1 - timeStamp0
t_units =attr(tdelta, "units")
if (BATmode){
 sink(fpathx, append=TRUE)
 cat("=== Script for `t_varsid=`", t_varsid, "Execution_time", tdelta, ", ", t_units, "\n")
 sink()
 tmp1 = paste0(tvarsx_tm, "_cva_glmnet.Rdata")
 tmp2 = paste0(tvarsx_tm, "_varia.Rdata") 
 
 sink(fpath_map)
  cat(t_varsid, ",", tmp1,  "\n")
  cat(t_varsid, ",", tmp2,  "\n")
 sink()


}

# Plots
# task_plot = task1$clone()

# if (length(event_lvls) == 2) autoplot(task_plot, type = "target") # Survival curve
# task_plot$select("BMI")
#autoplot(task_plot, type = "duo")
# autoplot(mylearner, type = "prediction", task1e) # not supported

 