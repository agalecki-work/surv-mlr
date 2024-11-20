
# 2.2 Learners

Learner # <Learner> object generator

mlr_learners
lrn()
lrn("regr.rpart")

# 2.2.1 Training

# load mtcars task
tsk_mtcars = tsk("mtcars")

# load a regression tree learner 
lrn_rpart = lrn("regr.rpart")
sort(names(lrn_rpart)) # ex. $feature_types, $packages, $properties, $predict_types, $param_set


# pass the task to the learner via $train()
lrn_rpart$train(tsk_mtcars)

# inspect the trained model
lrn_rpart$model

#- lrn_rpart$help()


# 2.2.1.1 Partitioning Data

splits = partition(tsk_mtcars)
splits



# 2.2.2 Predicting

prediction = lrn_rpart$predict(tsk_mtcars, row_ids = splits$test)
prediction

library(mlr3viz)
prediction = lrn_rpart$predict(tsk_mtcars, splits$test)
autoplot(prediction)

# Predict new data

mtcars_new = data.table(cyl = c(5, 6), disp = c(100, 120),
  hp = c(100, 150), drat = c(4, 3.9), wt = c(3.8, 4.1),
  qsec = c(18, 19.5), vs = c(1, 0), am = c(1, 1),
  gear = c(6, 4), carb = c(3, 5))
prediction = lrn_rpart$predict_newdata(mtcars_new)
prediction

