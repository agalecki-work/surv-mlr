rm(list=ls())

library(mlr3)
library(mlr3proba)

# Create a survival task
task <- TaskSurv$new("survival_lung", backend = survival::lung, 
                      time = "time", event ="status",
                      type = "right", # default
                      label = "Lung data (n=228)"
                      )
nms = sort(names(task)) #  ".__enclos_env__" -- "weights"

task_modes = sapply(nms, FUN= function(x) mode(task[[x]]))

mode(task$.__enclos_env__) # "environment"
mode(task$.backend) # "environment"
task$censtype  # right
task$col_hashes       # "1e2f8dce93c0dc46.age", "1e2f8dce93c0dc46.inst". ...

names(task$col_info)  # Column attributes
task$col_info$id      # colnames
names(task$col_roles) # [1] "feature" "target"  "name"    "order"   "stratum" "group"   "weight" 

task$extra_args       # list()
task$feature_names    # 
task$formula(rhs = NULL, reverse = FALSE) # Surv(time, status, type = "right") ~ .
task$groups           # NULL
task$hash             # "6bc5ddf5931e4326"
task$id               # "survival_lung"
task$label            # "Lung data (n=228)"
task$labels           # One label per each variable, ex: time = NA
task$man              # NA
task$properties       # character(0)
task$row_ids          # 1:228
task$row_names        # NULL
names(task$row_roles) # use
task$row_roles$use    # 1:228
task$strata           # NULL
task$task_type        # surv   
head(task$truth(rows = NULL)) # 306,455,  1010+,  210,   883,  1022+
task$weights          # NULL

