# 2.5.2 TaskClassif

dt = as.data.table(mlr_tasks)
table(dt$task_type) 
dt[task_type == "classif"]

# Create own classification task
as_task_classif(palmerpenguins::penguins, target = "species")

# Binary classification problem
tsk_sonar = tsk("sonar")
tsk_sonar

tsk_sonar$class_names

# multiclass problem

tsk_penguins = tsk("penguins")
tsk_penguins$properties
tsk_penguins$class_names

# Load the "Sonar" dataset from the "mlbench" package as an example
data(Sonar, package = "mlbench")
# specifying the positive class:
tsk_classif = as_task_classif(Sonar, target = "Class", positive = "R")
tsk_classif$positive

