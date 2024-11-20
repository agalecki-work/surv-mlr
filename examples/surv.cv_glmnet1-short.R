
# Load Libraries
packages <- c("mlr3", "mlr3proba", "mlr3learners", "mlr3extralearners", "glmnet", "mlr3pipelines")
lapply(packages, require, character.only = TRUE)
## Load the built-in pbc task
task = tsk("pbc")

## Define a pipeline with one-hot encoding preprocessing step using mlr3pipelines
po_onehot = po("encode", method = "one-hot")
## Define the learner and set parameters
learner0 = lrn("surv.cv_glmnet", alpha = 0.5)

# train the full pipeline
glrn0 = po_onehot %>>% po("learner", learner0)
learner = GraphLearner$new(glrn0)
learner$train(task)
# Verify the state of the graph
print(learner$graph)
