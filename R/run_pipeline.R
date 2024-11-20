
# Main processing function
run_pipeline <- function(data, time_col, status_col, admin_censor_time, split_ratio = 0.7) {
  
  # Apply administrative censoring
  data <- apply_admin_censoring(data, time_col, status_col, admin_censor_time)
  
  # Perform data split
  task = TaskSurv$new(id = "task", backend = data, time = time_col, event = status_col)
  holdout = rsmp("holdout", ratio = split_ratio)
  holdout$instantiate(task)
  train_set = holdout$train_set(1)
  test_set = holdout$test_set(1)
  
  # Separate training and testing data
  train_data <- data[train_set]
  test_data <- data[test_set]
  
  # Impute missing values
  target_cols <- c(time_col, status_col, "id", "weights")
  completed_train_data <- mice_impute_data(train_data, target_cols)
  completed_test_data <- mice_impute_data(test_data, target_cols)
  
  # Create TaskSurv objects
  task_train = create_surv_task(completed_train_data, time_col, status_col)
  task_test = create_surv_task(completed_test_data, time_col, status_col)
  
  # Define and train learner
  learner = lrn("surv.cv_glmnet")
  learner$param_set$values = list(alpha = 0.5, nfolds = 5)
  learner$train(task_train)
  
  # Predict and evaluate
  prediction = learner$predict(task_test)
  measure = msr("surv.cindex")
  score = prediction$score(measure)
  
  return(score)
}
