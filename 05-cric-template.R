### source("05-cric-main.R")

rm(list=ls())


# Load libraries
pkgs <- c("mlr3", "mlr3proba", "mlr3learners", "mlr3extralearners", "glmnet", "mlr3pipelines","dplyr", 
    "data.table", "Hmisc", "mice") # sas7bdat
lapply(pkgs, require, quietly =TRUE, character.only = TRUE)

# Load df_complete
load(file = "./results/cric_complete112023.Rdata", verbose=TRUE)
print(Info)
print(var_labels)
print(names(df_complete))

# `df_complete`: Convert to factors. Create weights, if needded <--- !!!
df_complete = within(df_complete,{
   ACRcat   = factor(ACRcat)
   CKD      = factor(CKD)
   CHF      = factor(CHF)
   AFIB     = factor(AFIB)
   RACE_CAT_1 = factor(RACE_CAT_1)
   wght     = 1.2
})


#=======  Project info

#-- Target vars `time` and `status` defined

t_vars  = c(time = "TIME_ESRD", status = "ESRD")

OLx         = paste0("OL", 1:21) # OL1,..., OL21
spline_vars = c("OL1", "OL2")
x_vars      = c("AFIB", "AGE_INTEGER", "BMI", "CHF")  
fac_vars    = c("ACRcat", "CKD", "RACE_CAT_1")


feature_vars = c(x_vars, fac_vars, OLx) 
feature_test = c(x_vars, fac_vars, "OL1", "OL2")

# Info list for TaskSurv with `df_origx` used as input dataset
Info_main = list( 
   data_orig     = c("df_complete", "./results/cric_olinknpx_112023_v1.Rdata"), # name, path
   id_col        = "PID",
   target_vars   = t_vars,
   feature_cols  = feature_test,
   weight_col    = "wght",
   time_horizon  = 7,
   spline_vars   = spline_vars
)
# rm(OLx, spline_vars,t1_vars, t2_vars, t3_vars,t4_vars, x_vars, fac_vars,feature_test, feature_vars)

id_col       = Info_main$id_col
time_col     = Info_main$target_vars[1]
time2_col    = Info_main$time2_col
event_col    = Info_main$target_vars[2]
feature_cols = Info_main$feature_cols
time_horizon = Info_main$time_horizon
weight_col   = Info_main$weight_col

# keep `t_vars` only (skip remaining t_vars
keep_vars = c(id_col, time_col, time2_col, event_col, feature_cols, weight_col)
setdiff(keep_vars, names(df_complete))
df_complete = df_complete[, keep_vars]


#========== Derive new vars/modify `df_complete`

#-- Admin censoring

apply_admin_censoring <- function(data, time_col, event_col, censor_time = Inf) {
    censor_timex = if (is.infinite(censor_time)) max(time_col) else censor_time 
    setDT(data)  # Ensure the data is a data.table
    data[, (event_col) := fifelse(get(time_col) > censor_time, 0, get(event_col))]
    data[, (time_col)   := pmin(get(time_col), censor_timex)]
    return(data)
}


df_complete = apply_admin_censoring(df_complete, time_col = time_col, event_col = event_col,
              censor_time = time_horizon)




task_cric = TaskSurv$new(id = paste0("task_", time_col), backend = df_complete, 
                             time = time_col, event = event_col)
task_cric$set_col_roles(id_col, roles = "group")
if (length(weight_col) ==1) task_cric$set_col_roles(weight_col, roles = "weight")
task_cric$set_col_roles(feature_cols, roles = "feature")



## Define a pipeline with one-hot encoding preprocessing step using mlr3pipelines
po_onehot_fac = po("encode", method = "one-hot", affect_columns = selector_type("factor"))

apply_splines = function(x) {
       as.data.table(splines::ns(x, df = 3))
      }
           
pom =  po("colapply", 
        id = "spline_all", 
        applicator = apply_splines, 
        affect_columns = selector_name(Info_main$spline_vars))  %>>% 
        po_onehot_fac %>>% po("mutate")    

   
### Mutate task_cric

task_mutate = pom$train(task_cric)$mutate.output
task_mutate

df_expanded = as.data.table(task_mutate)  # splines and factors expanded

task_


           








 