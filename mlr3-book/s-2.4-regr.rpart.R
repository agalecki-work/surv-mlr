# 2.4.  Our First Regression Experiment

library(mlr3)
set.seed(349)

# load and partition our task
tsk_mtcars = tsk("mtcars")
splits = partition(tsk_mtcars)  # ratio = 0.67)

# load featureless learner
lrn_featureless = lrn("regr.featureless")
# load decision tree and set hyperparameters
lrn_rpart = lrn("regr.rpart", cp = 0.2, maxdepth = 5)
# load MSE and MAE measures
measures = msrs(c("regr.mse", "regr.mae"))
# train learners
lrn_featureless$train(tsk_mtcars, splits$train)
lrn_rpart$train(tsk_mtcars, splits$train)
# make and score predictions
lrn_featureless$predict(tsk_mtcars, splits$test)$score(measures)

lrn_rpart$predict(tsk_mtcars, splits$test)$score(measures)