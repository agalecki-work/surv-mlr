# 2.3 Evaluation

library(mlr3)

lrn_rpart = lrn("regr.rpart")
tsk_mtcars = tsk("mtcars")
splits = partition(tsk_mtcars)
lrn_rpart$train(tsk_mtcars, splits$train)
prediction = lrn_rpart$predict(tsk_mtcars, splits$test)

# 2.3.1 Measures

mlr_measures # 65 stored values: aic, bic, classif.bbrier
msr()

all_msrs = as.data.table(msr())
all_msrs[, c("key","label", "task_type")]

measure = msr("regr.mae")
measure

