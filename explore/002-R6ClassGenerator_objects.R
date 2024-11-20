library(mlr3)
library(mlr3proba)

# List all objects in the mlr3 package
objects_in_mlr3 <- ls(c("package:mlr3","package:mlr3proba"))

# Filter out the R6ClassGenerator objects
R6_classes <- Filter(function(x) {
  obj <- get(x, envir = asNamespace("mlr3"))
  inherits(obj, "R6ClassGenerator")
}, objects_in_mlr3)

# Print the list of R6ClassGenerator objects
print(R6_classes)

print(mlr_task_generators)

