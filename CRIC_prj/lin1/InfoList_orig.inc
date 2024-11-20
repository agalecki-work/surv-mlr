#=======  Create `Info` list 

#---  Target vars `time` and `status` defined
#-    Note: event variable has two representations (numeric and factor)
t_vars01   = c(time= "TIME_ESRD", event= c(num= "ESRD"), id='01')
t_vars02   = c(time= "TIME_LOSS50", event= c(num= "LOSS50"), id='02')
t_vars03   = c(time= "TIME_LOSS50_ESRD", event= c(num= "LOSS50_ESRD"), id='03')
t_vars04   = c(time= "TIME_ESRD4", event= c(num= "ESRD4"), id='04')
t_vars05   = c(time= "TIME_ESRD7", event= c(num= "ESRD7"), id='05')
t_vars06   = c(time= "TIME_ESRD10", event= c(num= "ESRD10"), id='06')
t_vars07   = c(time= "TIME_LOSS50_ESRD4", event= c(num= "LOSS50_ESRD4"), id='07')
t_vars08   = c(time= "TIME_LOSS50_ESRD7", event= c(num= "LOSS50_ESRD7"), id='08')
t_vars09   = c(time= "TIME_LOSS50_ESRD10", event= c(num= "LOSS50_ESRD10"), id='09')
t_vars10   = c(time= "TIME_DEATH", event= c(num= "DEAD"), id='01')
     
t_vars31 = c(time= "eTIME_ESRD",         event= c(num= "event_ESRDx",        fac = "event_ESRDf"),   id='31') # fac= needed
t_vars32 = c(time= "eTIME_LOSS50",       event= c(num= "event_LOSS50x",      fac=  "event_LOSS50f"), id='32')
t_vars33 = c(time= "eTIME_LOSS50_ESRD",  event= c(num= "event_LOSS50_ESRDx", fac= "event_LOSS50_ESRDf" ), id='33')
t_vars34 = c(time= "eTIME_ESRD4",        event= c(num= "event_ESRD4x",       fac= "event_ESRD4f" ), id='34')
t_vars35 = c(time= "eTIME_ESRD7",        event= c(num= "event_ESRD7x",       fac= "event_ESRD7f"),  id='35')
t_vars36 = c(time= "eTIME_ESRD10",       event= c(num= "event_ESRD10x",      fac= "event_ESRD10f"), id='36')
t_vars37 = c(time= "eTIME_LOSS50_ESRD4", event= c(num= "event_LOSS50_ESRD4x",fac= "event_LOSS50_ESRD4f" ), id='37')
t_vars38 = c(time= "eTIME_LOSS50_ESRD7", event= c(num= "event_LOSS50_ESRD7x",fac= "event_LOSS50_ESRD7f"),  id='38')
t_vars39 = c(time= "eTIME_LOSS50_ESRD10",event= c(num= "event_LOSS50_ESRD10x",fac="event_LOSS50_ESRD10f"), id='39')

###t_vars    = t_vars32 # Choose one
crhs = paste0("t_vars", t_varsid)
t_vars = eval(parse(text= crhs))
OLx       = paste0("OL", 1:21) # OL1,..., OL21
x_vars    = c("AFIB", "AGE_INTEGER", "BMI", "CHF")  
fac_vars  = c("ACRcat", "CKD", "RACE_CAT_1")

feature_vars = c(x_vars, fac_vars, OLx) 
feature_test = c(x_vars, fac_vars, "OL1", "OL2")

# Info list 

nfolds = 5
glmnet_args         = list( s="lambda.min", nfolds = nfolds) # alpha will be added inside loop
ctree_args          = list()
cv_coxboost_args    = list()

Info = list(
#   analysis_lbl  = t_vars["time"],
   target_cols   = t_vars,
   feature_cols  = feature_vars,
   d_cols        = c(id = "PID"),            # id variable (mandatory) role "group" will be assigned
                    # weight = "wght",
                    # subcohort= "CHF",      # 0/1 variable for case-cohort study
                    # "log_OL1"),            # Declare additonal variables used in the analysis
   time_horizon  = 15,
   partition_ratio = 0.8, # split
   cvglmnet_info    = c(type = "surv.cv_glmnet", args = "glmnet_args"),
   learners_info    = c(surv.ctree = "ctree_args", surv.cv_coxboost = "cv_coxboost_args"),
#    subset         = "CKD == 'CKD3b'",
#   splined_vars   = c("OL1"),
   pen_factor_modify = c(BMI = 0, CKD. = 0), # Note dot after CKD (expanded factor),
   alphas         = c(a.25 =0.25, a.50= 0.5, a.75= 0.75, a.1 =1), 
   seed           = c(foldid = 1324, split = 7943) 
)

# check Info list
names_dcols0 = names(Info$d_cols)
names_dcols  = setdiff(names_dcols0, "")
cat("Info listcheck: Names of the elements in Info$d_cols vector (Note: id is mandatory) \n", names_dcols, "\n")
