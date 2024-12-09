OLx       = paste0("OL", 1:21) # OL1,..., OL21
x_vars    = c("AFIB", "AGE_INTEGER", "BMI", "CHF")  
fac_vars  = c("ACRcat", "CKD", "RACE_CAT_1")

feature_vars = c(x_vars, fac_vars, OLx) 
feature_test = c(x_vars, fac_vars, "OL1", "OL2")

# Info list 

nfolds = 5
glmnet_args         = list( s="lambda.min", nfolds = nfolds) # alpha argument will be added inside loop

Info = list(
   #subset        = "CKD == 'CKD3b'",
   time_horizon  = 10,
   feature_cols  = feature_vars,
   d_cols        = c(id = "PID"),            # named vector: id variable (mandatory) role "group" will be assigned
                   #  weight = "wght",     
                   #  subcohort= "CHF"),     # 0/1 variable for case-cohort study
   add_cols      = c("log_OL1"),            # Additonal features used in the analysis
   partition_ratio = 0.8,                   # training/test split
   cvglmnet_info   = c(type = "surv.cv_glmnet", args = "glmnet_args"),
   alphas         = c(a.25 =0.25, a.50= 0.5, a.75= 0.75, a.1 =1), 
   pen_factor_modify = c(BMI = 0, CKD. = 0), # Note dot after CKD (expanded factor),
   # splined_vars   = c("OL1"),
   seed           = c(foldid = 1324, split = 7943) 
)

