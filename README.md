# surv-mlr

# Get detailed information about a specific learner
learner_info = mlr_learners$get("surv.cv_glmnet")
print(learner_info)

# Print the parameter set
print(learner$param_set)

# Access specific information about learner parameters
param_info = mlr_learners$get("surv.cv_glmnet")$param_set
print(param_info)


# Links

[https://mlr3book.mlr-org.com/](mlr3 book)

[https://github.com/bblodfon/mlr3tests/tree/main](mlr3tests)
[https://github.com/ModelOriented/survex](survex)
[https://stackoverflow.com/questions/60620158/using-mlr3-pipelines-to-impute-data-and-encode-factor-columns-in-graphlearner](GraphicLearner)
[https://ocbe-uio.github.io/survomics/survomics.html
