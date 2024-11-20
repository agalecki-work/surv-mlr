
# 2.1 Tasks

library(mlr3)
mlr_tasks # <DictionaryTask>
tsk_mtcars = tsk("mtcars")
tsk_mtcars
names(tsk_mtcars)

# Create task
data("mtcars", package = "datasets")
mtcars_subset = subset(mtcars, select = c("mpg", "cyl", "disp"))
str(mtcars_subset)

tsk_mtcars = as_task_regr(mtcars_subset, target = "mpg", id = "cars")
tsk_mtcars

library(mlr3viz)
autoplot(tsk_mtcars, type = "pairs")

# Row IDs
task = as_task_regr(data.frame(x = runif(5), y = runif(5)),
  target = "y")
names(task)
task$row_ids



# 2.1.2 Retrieving Data

c(tsk_mtcars$nrow, tsk_mtcars$ncol)
c(Features = tsk_mtcars$feature_names,
  Target = tsk_mtcars$target_names)
  
#  row IDs are not the same as row numbers.
head(tsk_mtcars$row_ids)

# 2.1.3 Task mutators

tsk_mtcars_small = tsk("mtcars") # initialize with the full task
tsk_mtcars_small$select("cyl") # keep only one feature
tsk_mtcars_small$filter(2:3) # keep only these rows
tsk_mtcars_small$data() # two rows and two columns

# As R6 uses reference semantics (Section 1.5.1), you need to use $clone() 
#    if you want to modify a task while keeping the original object intact.
# the right way
tsk_mtcars = tsk("mtcars")
tsk_mtcars_right = tsk_mtcars$clone() # !!!! clone
tsk_mtcars_right$filter(1:2)
# original data unaffected
tsk_mtcars$head()
