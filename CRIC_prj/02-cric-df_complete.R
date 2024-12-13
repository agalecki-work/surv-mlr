### source("02-cric-df_complete.R")

#==== Script to perform missing values imputation 
# Info list: `Info_df_complete` defined inside this script

#---- Input: load(data_in["fpath"], verbose=TRUE)

#---- Output: save(file = save_path, list = keep_objects)

rm(list=ls())
prj_path = "."
# Load libraries
# packages <- c("mlr3", "mlr3proba", "mlr3learners", "mlr3extralearners", "glmnet", "mlr3pipelines",
pkgs = c("dplyr", "data.table", "Hmisc", "mice") # sas7bdat
tt = lapply(pkgs, require, quietly =TRUE, character.only = TRUE)

#=======  Project info

#-- Targets with `time` and `event` columns listed (they will be included in complete data)

t1_vars  = c("TIME_ESRD",  "ESRD")
t2_vars  = c("TIME_LOSS50","LOSS50")
t3_vars  = c("TIME_LOSS50_ESRD", "LOSS50_ESRD")
t4_vars  = c("TIME_DEATH", "DEAD")
t5_vars  = c("TIME_ESRD7", "ESRD7")
t6_vars  = c("TIME_ESRD10", "ESRD10")
t7_vars  = c("TIME_LOSS50_ESRD7","LOSS50_ESRD7")
t8_vars  = c("TIME_LOSS50_ESRD10","LOSS50_ESRD10")

t9_vars  = c("TIME_ESRD4", "ESRD4")
t10_vars  = c("TIME_LOSS50_ESRD4","LOSS50_ESRD4")

tvars_mtx2 = matrix(c(t1_vars, t2_vars, t3_vars, t4_vars, t5_vars, t6_vars, t7_vars, t8_vars, t9_vars, t10_vars), 
                ncol=2, byrow=TRUE)
colnames(tvars_mtx2) = c("time", "event")


#--- Auxiliary vectors used to define components in `Info_df_complete` list 
#--- xmiss variables to be used by mice() function
xmiss0 = c(
 "RACE_CAT_1", "Slope_EPI", "Slope_CRIC", "EGFR_CKD_EPI",      "CKD",      "AGE_INTEGER", "BMI", "UACRATIO", "SEX",                
 "ACRcat"    , "Log2UACR" , "eGFREPI_10", "HEMOGLOBIN_A1C_v5", "SMOKENOW", "MIREVASC"   , "PVD", "CHF",               
 "STROKE"    ,  "ANYCVD"  , "AFIB"      , "SYSTOLIC"         ,  "PCR_24HRURINE"   ,       "PCR_URINE_COMBINED")

OLx         = paste0("OL", 1:21) # OL1,..., OL21
xmiss =   c(xmiss0, OLx) #Olx variables always included

added_vars0 = c("PID") #, tvars_all) #
added_vars = c("<tvars>", added_vars0) # tvars are mandatory to include

# Info list 
Info_df_complete = list( 
   prj_lbl = paste0(prj_path, ": complete_df created"),
   data_in = c(name ="cric_olinknpx_112023_v1_sas7bdat", fpath ="./data/cric_olinknpx_112023_v1.Rdata"), # name, path
   tvars_mtx2 = tvars_mtx2,   # mtx with Surv target vars
   xmiss   = xmiss,           # predictors used by imputation
   added_vars= added_vars,    # target vars will be included by default
   save_path = paste0(prj_path, "02-cric_complete112023.Rdata") 
)
# rm(OLx, t1_vars, t2_vars, t3_vars,t4_vars, t5_vars)

# Unpack Info list

data_in    = Info_df_complete$data_in
tvars_mtx2 = Info_df_complete$tvars_mtx2
tvars_mtx3 = Info_df_complete$tvars_mtx3
xmiss      = Info_df_complete$xmiss
added_vars = Info_df_complete$added_vars
save_path  = Info_df_complete$save_path

tvars_all2 = tvars_mtx2 # vector
dim(tvars_all2) = NULL
# tvars_all  # vector
# t1_vars

tvars_all3 = tvars_mtx3 # vector
dim(tvars_all3) = NULL


#==== Load original data created using `read.sas7bdat()` function from `sas7bdat` library
load(data_in["fpath"], verbose=TRUE)

df_orig = eval(parse(text = data_in[1])) 
rm(list = data_in[1])

## Check for missing vals in `df_orig`
nmiss_orig = sapply(df_orig, function(x) sum(is.na(x)))
cat("Original data:", data_in[1], " (", nrow(df_orig), "x", ncol(df_orig), ")\n") 
cat("Total Number of missing values in original data", sum(nmiss_orig), "\n") 
nmiss_orig

#--- Extract var labels from `df_orig` (assuming `sas7bdat` was used)
col_info   = attr(df_orig, "column.info")
var_lbls = sapply(col_info, function(x) if (is.null(x$label)) x$name else x$label) 
names(var_lbls) = names(df_orig)
var_labels = data.frame(labels = var_lbls)

# `mice_nms` contains variable names to be used by mice() 
mice_nms = c(xmiss, added_vars0)
mice_nms = unique(c(mice_nms, tvars_all2, tvars_all3))
excluded_vars = setdiff(names(df_orig), c(mice_nms, tvars_all2, tvars_all3))
common_vars = intersect(xmiss, added_vars)
Info_df_complete$excluded_vars  = excluded_vars
Info_df_complete$common_vars    = if (length(common_vars) == 0) NULL else common_vars

# df2 prepared for mice()
df2 = df_orig[, mice_nms]

ini   = mice(df2, maxit=0) # dry run without iterations to get the predictor matrix
predx = ini$predictorMatrix # is your predictor matrix
predx[,added_vars0] = 0 # set column values to zero to exclude them as predictors

m1 <- mice(df2, m=1, maxit = 5, method = 'pmm', seed = 123, pred = predx) # use the new matrix in mice
df_complete = complete(m1)

# Check for missing values

## Check for missing vals
nmiss_complete = sapply(df_complete, function(x) sum(is.na(x)))
cat("Complete data for:", data_in[1], " (", nrow(df_complete), "x", ncol(df_complete), ")\n") 
cat("Total Number of missing values in `df_complete`", sum(nmiss_complete), "\n") 

keep_objects   = c("Info_df_complete", "var_labels", "df_complete")

save(file = save_path, list = keep_objects)


# Cleanup (No changes below)
ls_objects <- ls()
rm_objects  <- c(setdiff(ls_objects, keep_objects), "ls_objects")
rm(list = rm_objects)
rm(rm_objects)


