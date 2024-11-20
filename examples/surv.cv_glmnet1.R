## source("surv.cv_glmnet1.R")

# Load Libraries
packages <- c("mlr3", "mlr3proba", "mlr3learners", "mlr3extralearners", "glmnet", "mlr3pipelines")
lapply(packages, require, character.only = TRUE)
rm(list = ls())

## Load and preprocess the built-in pbc task
task = tsk("pbc")
print(task)

# Checking for and filtering out incomplete cases 
ids = complete.cases(task$data())
sum(!ids)  # Output the number of incomplete observations
task$filter(which(ids))

# Print the task data before processing
print(head(task$data()))

## Define a pipeline with one-hot encoding preprocessing step using mlr3pipelines
po_onehot = po("encode", method = "one-hot")

# Encode the data manually for debugging
encoded_task = po_onehot$train(list(task))$output
print(head(encoded_task$data()))  # Inspect the encoded data

## Define the learner and set parameters
learner0 = lrn("surv.cv_glmnet", alpha = 0.5)

# Manually train glmnet with the encoded data
data_encoded = encoded_task$data()
print(head(data_encoded))

x = model.matrix(~ . - 1, data = data_encoded[, -c("time", "status")])
y = survival::Surv(data_encoded$time, data_encoded$status)
print(head(x))
print(head(y))

# Train glmnet manually to check for issues
tryCatch({
  cvfit = cv.glmnet(x, y, family = "cox", alpha = 0.5)
  print(cvfit)
  plot(cvfit)
}, error = function(e) {
  cat("Manual training error: ", e$message, "\n")
  quit(status = 1)
})

# Now attempt to train the full pipeline
glrn0 = po_onehot %>>% po("learner", learner0)
learner = GraphLearner$new(glrn0)
options("mlr3.log_console" = TRUE)

# Train the GraphLearner
tryCatch({
  learner$train(task)
  cat("GraphLearner Training completed.\n")
}, error = function(e) {
  cat("Error during GraphLearner training: ", e$message, "\n")
})

# Verify the state of the graph
print(learner$graph)

# Check if the model is trained and exists
if (!is.null(learner$graph$pipeops$`learner.cv_glmnet`$learner$model)) {
  # Extract the trained model (cv.glmnet object)
  trained_cv_glmnet = learner$graph$pipeops$`learner.cv_glmnet`$learner$model
  print(trained_cv_glmnet)
  plot(trained_cv_glmnet$glmnet.fit)
  
  # Predict on the same task (for demonstration purposes)
  prediction = learner$predict(task)
  time_point = 600
  surv_distr = prediction$distr
  survival_probs = surv_distr$survival(time = time_point)
  risk = 1 - survival_probs
  print(risk[1, 1:40])
} else {
  cat("Model training failed or model not found.\n")
}