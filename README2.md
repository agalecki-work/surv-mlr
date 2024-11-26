
# Get detailed information about a specific learner
learner_info = mlr_learners$get("surv.cv_glmnet")
print(learner_info)

# Print the parameter set
print(learner$param_set)

# Access specific information about learner parameters
param_info = mlr_learners$get("surv.cv_glmnet")$param_set
print(param_info)

