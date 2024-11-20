nfolds = 5
glmnet_args         = list( s="lambda.min", nfolds = nfolds) # alpha will be added inside loop
ctree_args          = list()
cv_coxboost_args    = list()

prj_Info = list(
  original_Rms     = c(df= "cric_olinknpx_112023_v1_sas7bdat", Rdata= "cric_olinknpx_112023_v1.Rdata"),
  complete_nms     = c(df= "df_complete", Rdata= "02-cric_complete112023.Rdata"), # in data folder
  d_cols           = c(id = "PID"),            # id variable (mandatory) role "group" will be assigned
                      # weight = "wght",
                      # subcohort= "CHF",      # 0/1 variable for case-cohort study
                      # "log_OL1"),            # Declare additonal variables used in the analysis
  #   splined_vars   = c("OL1"),
    seed           = c(foldid = 1324, split = 7943), 
  add_vars_name    = "cric_add_variables.inc",
  subset         = "CKD == 'CKD3b'",
  time_horizon     = 15,
  partition_ratio  = 0.8, # split
  alphas           = c(a.25 =0.25, a.50= 0.5, a.75= 0.75, a.1 =1), 
  cvglmnet_info    = c(type = "surv.cv_glmnet", args = "glmnet_args"),
  learners_info    = c(surv.ctree = "ctree_args", surv.cv_coxboost = "cv_coxboost_args")
)  
# check Info list
names_dcols0 = names(prj_Info$d_cols)
names_dcols  = setdiff(names_dcols0, "")
cat("Info listcheck: Names of the elements in prj_Info$d_cols vector (Note: id is mandatory) \n", names_dcols, "\n")

  