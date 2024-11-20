# source("025-multiple_learners.R")

rm(list=ls())
library(mlr3)
library(mlr3proba)
library(mlr3learners)
library(mlr3extralearners)
library(paradox)
library(glmnet)

library(data.table)

# Convert the lung dataset to a `data.table`
lung_data = as.data.table(survival::lung)

# Convert factors to numeric (columns `sex`, `ph.ecog`)
lung_data[, sex := as.numeric(sex)]
lung_data[, ph.ecog := as.numeric(ph.ecog)]

# Remove rows with any missing values
lung_data = na.omit(lung_data)

# Create a survival task
task = TaskSurv$new("lung", lung_data, time= "time", event= "status")

#--- Perform Initial Holdout Splitting
set.seed(42)  # for reproducibility
holdout_resampling = rsmp("holdout")
holdout_resampling$instantiate(task)

train_indices = holdout_resampling$train_set(1)
test_indices = holdout_resampling$test_set(1)

train_task = task$clone(deep = TRUE)$filter(train_indices)
test_task = task$clone(deep = TRUE)$filter(test_indices)

#-- Learners

learner_list = list(
  lrn("surv.coxph"),
  lrn("surv.cv_glmnet"),

  lrn("surv.ranger", num.trees = 100)
)

##learner_list[[2]]$param_set$values$foldid = fold_ids
set.seed(42)  # for reproducibility
cv_folds = 3
n_train = train_task$nrow
fold_ids = sample(rep(seq(cv_folds), length.out = n_train))
# Define a custom resampling strategy
# 
train_sets = lapply(seq(cv_folds), function(x) which(fold_ids != x))
test_sets  = lapply(seq(cv_folds), function(x) which(fold_ids == x))

custom_resampling = rsmp("custom")
custom_resampling$instantiate(task, train_sets= train_sets, test_sets=test_sets)

# Modify learners to ensure that foldid is used by surv.cv_glmnet

measures = msrs(c("surv.cindex", "surv.brier", "surv.dcalib"))
measure_surv_cindex = msr("surv.cindex")
measure_surv_brier = msr("surv.brier")
# Pre-allocate a list with the number of learners
all_results = vector("list", length(learner_list))
cv_glmnet_results = NULL

# Loop over learners
for (i in seq_along(learner_list)) {
  learner = learner_list[[i]]
 if (inherits(learner, "LearnerSurvCVGlmnet")) {
     cat("-- aurv.cv_glmnet --- Start","\n")
 
    learner$param_set$values$foldid = fold_ids
    train_data = train_task$data()
    test_data = test_task$data()
    
    # Ensure that relevant columns are numeric
    feature_cols = colnames(train_data)[!colnames(train_data) %in% c("time", "status")]

    y_train = as.matrix(train_data[, .(time, status)])
    x_train = as.matrix(train_data[, ..feature_cols, with = FALSE])
    mod = cv.glmnet(x_train, y_train, family = "cox", foldid = fold_ids)
    
    # Generating predictions on the test set
    x_test = as.matrix(test_data[, ..feature_cols, with = FALSE])
    y_test = as.matrix(test_data[, .(time, status)])
    preds_test = predict(mod, newx = x_test, s = "lambda.min")
    cindex_test = measure_surv_cindex$score(preds_test, y_test)
    brier_test = measure_surv_brier$score(preds_test, y_test)
    cv_glmnet_results = data.table(surv.cindex = cindex_test, surv.brier = brier_test, model = "surv.cv_glmnet")
    cat("-- aurv.cv_glmnet --- endif","\n")
   } else {
     model_name = learner$id
     cat("---", model_name, " START \n")

    # Perform custom resampling for other learners
    rr = resample(task, learner, custom_resampling, store_models = TRUE)
 results = rr$aggregate(measures)

  # Add model name to results
  # results[, model := model_name]
  all_results[[i]] = results
  cat("---", model_name, " endif \n")
  }
} # fir i

# Combine results into a single data table
# combined_results = rbindlist(all_results, fill = TRUE)

print(combined_results)
