# source("001Dictionaries.R")

rm(list =ls())

# list of loaded packages

message("--- list of loaded packages")
print(sort(loadedNamespaces()))

#---- List of available learners

message(" --- List of learners")
library(mlr3)
library(mlr3proba)
library(mlr3extralearners)
print(lrn()) # <DictionaryLearner>
available_learners <- as.data.table(mlr_learners)[, c("key", "label","task_type","predict_types")]
colnames(available_learners)


#--- List of available tasks

message("--- List of tasks")
print(tsk()) # <DictionaryTask> 
available_tasks <- as.data.table(mlr_tasks)
colnames(available_tasks)
available_tasks[task_type == "surv", c("key")]

# Task instance

tt = tsk("pbc")
ids = complete.cases(tt$data())

# number of incomplete observations
sum(!ids)

tt$filter(which(ids))

#--- Measures

message("--- List of measuress")
print(mlr_measures)
available_measures <- as.data.table(mlr_measures) # <DictionaryMeasure>
colnames(available_measures)

#----- List of available filters

message("--- List of filters")
library(mlr3filters)

print(flt()) # <DictionaryFilter>
available_filters <- as.data.table(mlr_filters)
colnames(available_filters)
flt("importance")$help()

#--- resamplings

message("--- List of resamplings")

print(mlr_resamplings) # <DictionaryResampling>
available_resamplings  <- as.data.table(mlr_resamplings)

#--- terminators

message("--- List of terminators")
library(mlr3tuning)
print(mlr_terminators) # <DictionaryTerminator>
available_terminators = as.data.table(mlr_terminators)

message("--- List of tuners")
print(mlr_tuners) # <DictionaryTerminator>
available_tuners = as.data.table(mlr_tuners)


# ========= List all available PipeOps

message("--- List of PipeOps")
library(mlr3pipelines)
print(mlr_pipeops)     #  <DictionaryPipeOp>
class(mlr_pipeops)    #  "DictionaryPipeOp" "Dictionary" "R6" 
available_pipeops   = as.data.table(mlr_pipeops)
colnames(available_pipeops)


# Select and print details for specific types of PipeOps (e.g., mutation)
message("--- Details of mutate PipeOps")
mutating_pipeops <- available_pipeops[grepl("mutate", key)]
mutating_pipeops_vec <- unlist(mutating_pipeops[, "key"])
names(mutating_pipeops_vec) <- NULL
print(mutating_pipeops_vec)

# Select and print details for specific types of PipeOps (e.g., impute)
message("--- Details of `impute` PipeOps")
imputing_pipeops <- available_pipeops[grepl("impute", key)]
imputing_pipeops_vec <- unlist(imputing_pipeops[, "key"])
names(imputing_pipeops_vec) <- NULL
print(imputing_pipeops_vec)


#--- Inspect Detailed Information for a Specific PipeOp

# Create an instance of a specific PipeOp, e.g., "colapply"
pipeop_instance <- po("colapply")  # mutate,colapply
# print(pipeop_instance)
colnames(pipeop_instance)

# To inspect all parameters of 'PipeOpMutate'
params <- pipeop_instance$param_set
print(params)
names(params)

# # Get the PipeOps dictionary
pipeops = mlr_pipeops


imputers = grep("impute", pipeops$keys(), value = TRUE)
print(imputers)


tsk_mtcars = tsk("mtcars")
print(tsk_mtcars)
class(tsk_mtcars)  # "TaskRegr", "TaskSupervised", "Task", "R6"
?mlr_tasks_mtcars
# tsk("mtcars")$help()

