### source("30-cric-cv.coxnet.R")

rm(list=ls())

BATmode = FALSE # TRUE # !!!

logx = NULL

# ===== Batch mode switch

if (BATmode){ # Multiple t_vars specified in args[2] will be processed in a batch mode
  args <- commandArgs(trailingOnly = TRUE)
  argsx = args[1]  # Ex: {%prj_name%:%anl_name%:%scriptBaseName%} CRIC_prj:test:27-cric-cv.coxnet
  t_varsid  = args[2]
  txt = paste0("* ====== Script executed in batch mode for tvar=", t_varsid)
} else {
  argsx  ="CRIC_prj:test:30-cric-cv.coxnet"
  t_varsid = "01" # Ex. 02, 03, 31
  txt= paste0("* ====== Script executed from R console for tvar=", t_varsid)
}
  cat( txt, "\n")
  logx = c(logx,txt)

   csplit =  unlist(strsplit(argsx,":", fixed=TRUE))
   prj_name        = csplit[1] # Ex. CRIC_prj
   anl_name        = csplit[2] # Ex. test
   scriptBaseName  = csplit[3] # Ex. 27-cric-cv.coxnet-withFG
   prj_path        = paste0("./",prj_name) 
   
 timeStamp0 = Sys.time()
 txt =paste0("* Time stamp: ",timeStamp0)
  cat( txt, "\n")
  logx = c(logx,txt)
  print(as.POSIXlt(timeStamp0))


  txt=paste0("* == Project name: ", prj_name)
  cat( txt, "\n")
  logx = c(logx,txt)

# ====== Load libraries
pkgs <- c("mlr3", "mlr3proba", "mlr3learners", "mlr3extralearners", "glmnet", "mlr3pipelines","dplyr", 
    "data.table", "mlr3viz","survival", "dplyr","openxlsx", "utilsag","tidyr") #
res = lapply(pkgs, require, quietly =TRUE, character.only = TRUE, warn.conflicts =TRUE )
cat("--- Packages loaded \n")


# ==== Source R functions
source("./R/accept_col_roles.R")

# === Create `prj_Info list`
fpath2 = paste0(prj_path, "/prj_Info.inc")
source(fpath2) # 
# names(prj_Info) # 

nms0 = names(prj_Info)
txt0 =paste(nms0, collapse= "`, `")
txt = paste0("* Sourcing `", fpath2, ". `prj_Info` list with `", txt0, "` components created.")
cat(txt, "\n")
logx = c(logx, txt)


#=====  Load list with the results from 02*.R script, including `df_complete` data frame
tmp = prj_Info$complete_nms["Rdata"]
fpath = paste0(prj_path, "/data/", tmp)  
loaded_objects = load(file = fpath)
tmpx = prj_Info$complete_nms["df"]
assign("df_complete", eval(parse(text= tmpx )))
txt1 = paste(loaded_objects, collapse =", ")
txt = paste0("* Objects: ", txt1, " loaded from: ./data/", tmp) 
cat(txt, "\n")
logx = c(logx, txt)

# print(Info_df_complete)
# print(var_labels)
nms_df_complete = names(df_complete)

#=======  Create `Info` list 
fpath = paste0(prj_path,"/_Info/", anl_name, ".inc")
source(fpath)

#---- Extract info from `Info` list and create auxiliary vectors
nm_id = Info$d_cols["id"]
dcol_nms =names(Info$d_cols)
dcol_nms =Info$d_cols[!dcol_nms == ""]
mtx2 =cbind(names(Info$d_cols), Info$d_cols)
tmp0 =apply(mtx2,1,FUN = function(x) paste(x, collapse=" = ")) # vector
dcol_txt = paste(tmp0, collapse= "`, `")

txt = paste0("* == Info$d_col variables: `", dcol_txt,"`")
cat(txt, "\n")
logx=c(logx, txt)


#=======  Create `tvars_mtx` matrix 
fpath = paste0(prj_path, "/cric_tvars_mtx.inc")
source(fpath)

txt = paste0("* Sourcing ", fpath, ". `cric_tvars_mtx` with ", nrow(tvars_mtx), " rows created")
cat(txt, "\n")
logx = c(logx, txt)

# ====== Create `Info_tvars` auxiliary list with info on a selected `t_vars_id` 
# Select one: t_varsid; Ex. "01", "02","31" 
crhs = paste0("t_vars", t_varsid)
t_vars = eval(parse(text= crhs)) # `t_vars` is named vector with elements: id, time, event.num (and event.fac?)

Info_tvars = list(target_cols   = t_vars)
   
### ==== Create `analysis summary` tibble based on loaded Info objects

ctmp_rat0 = if (!is.na(Info$partition_ratio)) as.character(Info$partition_ratio) else 
   paste0 ("External validation: ", Info$d_cols["external_01"])

ctmp_rat = if (is.null(Info$d_cols["external_01"])) "Not assigned" else ctmp_rat0

CCH = if ("subcohort" %in% names(dcol_nms)) TRUE else FALSE

anl_summ = tribble(
 ~INFO, ~VALUE,
 "Prj_name: ", prj_name,
 "Time stamp: ", format(timeStamp0),
 "Data subset:", if (is.na(Info$subset))  "Not applicable" else Info$subset,
 "Time horizon:", as.character(Info$time_horizon),
 "Design cols:",  paste0("`", dcol_txt, "`"),
 "Partition ratio:",ctmp_rat,
 "CCH:", as.character(CCH)
)

dfinfo = data.frame( INFO =paste0("tmvar_", names(t_vars)), VALUE= t_vars)
rownames(dfinfo) = NULL
anl_summary= bind_rows(anl_summ, dfinfo)
txt = "* --- `analysis summmary` tibble created"
cat(txt, "\n")
logx= c(logx, txt)

#=== Source script to add new (or modify existing) variables in `df_complete` (add as many vars as you wish) 
nms_df_complete0 = names(df_complete)
add_vars_inc = prj_Info$add_vars_name
add_vars_path = paste0(prj_path,"/", add_vars_inc)
source(add_vars_path)
txt = paste0("* Sourcing ",add_vars_path, " done ... OK" ) 
cat(txt, "\n")
logx = c(logx, txt)


nms_df_complete = names(df_complete)
vars_added = setdiff(nms_df_complete, nms_df_complete0)

txt = paste0("* == Var names added to `df_complete` are stored in `vars_added` vector (n=", length(vars_added), ")" ) 
cat(txt, "\n")
logx = c(logx, txt)

# ==== Process `Info$dcols` vector
   
# check `Info$dcols` vector
names_dcols0 = names(Info$d_cols)
names_dcols  = setdiff(names_dcols0, "")
tmp0 = paste(names_dcols, collapse ="`, `") 
txt= paste0("* Info list: Names of the elements in Info$d_cols vector `", tmp0, "`")
cat(txt, "\n")
logx = c(logx, txt)

# ==== Subsetting `df_complete`

txt = paste0("* Subset `df_complete` (optional): ", Info$subset)
cat(txt, "\n")
logx=c(logx, txt)

txt= paste0("* `df_complete` before subset nrows =",  nrow(df_complete))
cat(txt, "\n")
logx=c(logx, txt)

esubset = parse(text= Info$subset)
df_complete = subset(df_complete, subset= eval(esubset))
txt= paste0("* `df_complete` after subset nrows =",  nrow(df_complete))
cat(txt, "\n")
logx=c(logx, txt)


# === `Info_tvars`
time_nm  = Info_tvars$target_cols["time"]      # mandatory string
evnt_num = Info_tvars$target_cols["event.num"] # mandatory string
evnt_fac = Info_tvars$target_cols["event.fac"] # character string or NA
tmp_id   = Info_tvars$target_cols["id"]
txt = paste0("* == time var: id =", tmp_id , ". Time var name = `", time_nm, "` , status =`", evnt_num, "`" )
cat(txt, "\n")
logx=c(logx, txt)

if (!is.na(evnt_fac)){
 event_fac = df_complete[[evnt_fac]]           # factor maybe needed 
 event_lvls = levels(event_fac)
 event_table = table( df_complete[[evnt_fac]])
 cat("--- Table for event factor: `", evnt_fac, "` \n", event_lvls , "\n", event_table, "\n" )
} else {
 event_lvls = c("0-censored", paste0("1-", evnt_num))
 event_table = table( df_complete[[evnt_num]]) # numeric event
 cat("--- Table for event variable: `", evnt_num, "` \n" , event_table, "\n" )
}




#====  Keep selected columns in `df_complete`
source("./src/02keep_selected_cols.R")
txt = paste0("* --- Selected columns in `df_complete`. Check `keep_vars`: with ", length(keep_vars), " elements.")
cat(txt, "\n")
logx=c(logx, txt)
 
#==== Admin censoring at Info$time_horizon
source("./R/apply_admin_censoring.R")
max_obstime = max(df_complete[[time_nm]])
txt= paste0("* Admin censoring at ", Info$time_horizon, " years (maximum observed time=", round(max_obstime, 2) , ")")
cat(txt, "\n")
logx= c(logx, txt)

df_complete = apply_admin_censoring(df_complete, time_col = time_nm, 
                event_col = evnt_num,
                censor_time = Info$time_horizon)
                               
# Note: `df_complete`  Contains both event.num and event.fac variables



##==== Define `pom` object of Graph class

source("./src/create_pomGraph_object.R")
 
txt = paste0("* --- Event variable has ", length(event_lvls), " levels")
cat(txt, "\n")
logx= c(logx, txt)

#====  Create `task1`
if (CCH) source("./src/create_task1_cch.R") else source("./src/create_task1.R")
txt =paste0("* --- mlr3:task1 created. CCH is ", CCH, " rat=", rat)
cat(txt, "\n")
logx= c(logx, txt)



# ==== Create `task1e` (expanded factors/splines
source("./src/create_task1e.R")
txt = "* --- mlr3:task1e created"
cat(txt, "\n")
logx= c(logx, txt)

 feature_nms0 = names(task1e$data()) # using df_expanded_num
 feature_nms  = setdiff(feature_nms0, c(time_nm, evnt_num, evnt_fac)) # Time/event vars removed



#==== Process learners  ============


cvglmnet_info = Info$cvglmnet_info
cvglmnet_args = get(cvglmnet_info["args"])

#====  `surv.cv_glmnet` learner 
if (Info$cvglmnet_info["type"] == "surv.cv_glmnet"){
 txt = "* -- Learner `surv.cv_glmnet` processed"
 cat(txt, "\n")
 logx= c(logx, txt)
 source("./src/surv.cv_glmnet_src.R")
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
 } #  for i in seq_along(learners_info))
 

     pred2_scoreList = lapply(pred2_scores, FUN = function(x) as_tibble(t(x)))
     names(pred2_scoreList) = learner_nms
     pred2_scoreAll = bind_rows( pred2_scoreList, .id = "learner_lbl")
     pred2_scoreAll["alpha_lbl"] = ""
     pred_scoreAll = rbind(pred_scoreAll, pred2_scoreAll) 
    
    save_objects = c("prj_Info", "Info","logx",save_objects0)
     fpath = paste0(prj_path,"/", anl_name, "/", tvarsx_id , tvarsx_tm,  "_varia.Rdata")
     save(list =save_objects, file =fpath)
 
     txt0 = paste(save_objects, collapse=", ")
     txt  = paste0(": Objects: `", txt0, "` saved_in `", fpath, "`")
     cat(txt, "\n")
     logx = c(logx, txt)
 
     pred_scoreAll =  pred_scoreAll %>% relocate(learner_lbl, .before = alpha_lbl)
     addWorksheet(wb, "pred_scoreAll")
     writeData(wb, "pred_scoreAll", pred_scoreAll)

} # if length(event_lvls) == 2


  
   
   fpath0 = paste0(prj_path,"/", anl_name, "/", tvarsx_id , tvarsx_tm)
   fpathx =paste0(fpath0, "_summary.xlsx")
   saveWorkbook(wb, fpathx, overwrite = TRUE)
   
   sheet_nms =getSheetNames(fpathx)
   txt0 = paste(sheet_nms, collapse = ", ")
   txt  = paste0("* Sheet_names: ", txt0, " in ", fpathx)
   cat(txt, "\n")
   logx= c(logx, txt)
   
timeStamp1 = Sys.time()
cat("Time stamp. t_varsid: ", t_varsid, " \n")

print(as.POSIXlt(timeStamp1))

fpathx = paste0("./", scriptBaseName, "x.log")  
fpath_map = paste0("./",prj_name, "/", anl_name, "/_map.log")
tdelta = timeStamp1 - timeStamp0
t_units =attr(tdelta, "units")


if (BATmode){
 sink(fpathx, append=TRUE)
 txt= pasye0("* Sink file appended in batch mode: ", fpathx)
 cat(txt, "\n")
 logx = c(logx, txt)
 
 txt = paste0("=== Script for `t_varsid=`", t_varsid, "Execution_time", tdelta, ", ", t_units, "\n")
 cat(txt, "\n")
 logx = c(logx, txt)
 sink()
 
 tmp1 = paste0(tvarsx_tm, "_cva_glmnet.Rdata")
 tmp2 = paste0(tvarsx_tm, "_varia.Rdata") 
 
 # `_map.log` xreated in analysis folder
 sink(fpath_map)
  cat(t_varsid, ",", tmp1,  "\n")
  cat(t_varsid, ",", tmp2,  "\n")
 sink()


} # if (BATmode)

# Plots
# task_plot = task1$clone()

# if (length(event_lvls) == 2) autoplot(task_plot, type = "target") # Survival curve
# task_plot$select("BMI")
#autoplot(task_plot, type = "duo")
# autoplot(mylearner, type = "prediction", task1e) # not supported

 