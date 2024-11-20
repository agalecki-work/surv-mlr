### source("52-accord-df_complete.R")

rm(list=ls())

# Load libraries
# packages <- c("mlr3", "mlr3proba", "mlr3learners", "mlr3extralearners", "glmnet", "mlr3pipelines",
pkgs = c("dplyr", "data.table", "Hmisc", "mice") # sas7bdat
lapply(pkgs, require, quietly =TRUE, character.only = TRUE)

#=======  Project info

#-- Targets with `time` and `event` columns listed (they will be included in complete data)

t1_vars = c("YRS_PRIMARY", "PRIMARY")
t2_vars = c("YRS_CASE40_JUN", "CASE40_JUNE")
t3_vars = c("YRS_DECLINE40_PLUS", "DECLINE40_PLUS")
t4_vars = c("FU_TM_ACCORDION", "TM_ACCORDION")
tvars_mtx2 = matrix(c(t1_vars, t2_vars, t3_vars, t4_vars), 
                ncol=2, byrow=TRUE)
colnames(tvars_mtx2) = c("time", "event")




#--- Auxiliary vectors used to define components in `Info_df_complete` list 

bmq_vars = paste("BMQ_", 1:21, sep="")

bm_vars = paste("BM", 1:21, sep="")
ccn_vars = paste("CCN", 1:7, sep="")

id_vars =c("MASKID", "BSI_ID", "PLATE")
dx_vars =c("BPTRIAL", "BPINT", "GLYINT", "FIBRATE") 

#--- xmiss variables to be used by mice() function
xmiss0 = c(
 "RACE_CAT_1", "Slope_EPI", "Slope_CRIC", "EGFR_CKD_EPI",      "CKD",      "AGE_INTEGER", "BMI", "UACRATIO", "SEX",                
 "ACRcat"    , "Log2UACR" , "eGFREPI_10", "HEMOGLOBIN_A1C_v5", "SMOKENOW", "MIREVASC"   , "PVD", "CHF",               
 "STROKE"    ,  "ANYCVD"  , "AFIB"      , "SYSTOLIC"         ,  "PCR_24HRURINE"   ,       "PCR_URINE_COMBINED")

xmiss0 = c("SUBCO15" ,"AGE", "FEMALE", "BMI", "HBA1C", "BASE_GFR", "GFR_F04", "LOG_BASE_UACR") 

xmiss =   c(xmiss0, bm_vars) # BM_vars always included

added_vars0 = c("MASKID","BSI_ID", "PLATE", "BPTRIAL", "BPINT", "GLYINT", "FIBRATE", ccn_vars) #, tvars_all) #
added_vars = c("<tvars>", added_vars0) # tvars are mandatory to include

# Info list 
Info_df_complete = list( 
   data_in = c(obj_name= "olink_analytical_dataset011123", fpath = "./data/olink_analytical_dataset011123.Rdata"), # name, path
   tvars_mtx2 = tvars_mtx2,   # mtx with Surv target vars
   xmiss   = xmiss,           # predictors used by imputation
   added_vars= added_vars,    # target vars will be included by default
   save_path = "./results/cric_complete112023.Rdata" 
)
# rm(OLx, t1_vars, t2_vars, t3_vars,t4_vars, t5_vars)

# Unpack Info list

data_in    = Info_df_complete$data_in
tvars_mtx2 = Info_df_complete$tvars_mtx2

xmiss      = Info_df_complete$xmiss
added_vars = Info_df_complete$added_vars
save_path  = Info_df_complete$save_path

tvars_all2 = tvars_mtx2 # vector
dim(tvars_all2) = NULL
# tvars_all  # vector
# t1_vars


#==== Load original data created using `read.sas7bdat()` function from `sas7bdat` library
load(data_in["fpath"], verbose=TRUE)

df_orig = eval(parse(text = data_in[1])) 
rm(list = data_in["obj_name"])

## Check for missing vals in `df_orig`
nmiss_orig = sapply(df_orig, function(x) sum(is.na(x)))
cat("Original data:", data_in[1], " (", nrow(df_orig), "x", ncol(df_orig), ")\n") 
cat("Total Number of missing values in original data: ", sum(nmiss_orig), "\n") 

#--- Extract var labels from `df_orig` (assuming `sas7bdat` was used)
col_info   = attr(df_orig, "column.info")
var_lbls   = sapply(col_info, function(x) if (is.null(x$label)) x$name else x$label) 
names(var_lbls) = names(df_orig)
var_labels = data.frame(labels = var_lbls)

# `mice_nms` contains variable names to be used by mice() 
mice_nms = c(xmiss, added_vars0)
mice_nms = unique(c(mice_nms, tvars_all2))
excluded_vars = setdiff(names(df_orig), c(mice_nms, tvars_all2))
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

cat("--- Excluded vars", "\n")
print(excluded_vars)

keep_objects   = c("Info_df_complete", "var_labels", "df_complete")

save(file = save_path, list = keep_objects)

# Cleanup (No changes below)
ls_objects <- ls()
rm_objects  <- c(setdiff(ls_objects, keep_objects), "ls_objects")
rm(list = rm_objects)
rm(rm_objects)


