#source("tt.R")

rm(list=ls())

library(data.table)
library(survival)
library(mlr3)
library(mlr3proba)
library(mlr3extralearners)

# Create a dummy dataset
set.seed(42)
nx =11
dummy_data <- data.table(
  ssid = 1:nx,
  time_col  = c(2, 1, 3, 3, 2, 4, 5,5,4,4,6),
  event_col = c(1, 0, 2, 1, 0, 2,1,1,1,0,2),
  x1 = sort(rnorm(nx)),
  x2 = runif(nx)
)
dtx = within(dummy_data, {        event_col = factor(event_col)
        })
dtx

# Transform the data using Fine-Gray
fg_data <- finegray(Surv(time_col, event_col) ~ ., data = dtx, etype = 1, id=ssid)
fg_data <- as.data.table(fg_data)


# Original task
task1 <- TaskSurv$new(id = "task1", backend = dtx, time = "time_col", event = "event_col", type="mstate")
task1$select(c("x1", "x2"))
task1

# Fine-Gray task
task1_finegray <- TaskSurv$new(id = "task1_finegray", backend = fg_data, time = "fgstart", 
          time2 = "fgstop", event = "fgstatus",
          type = "counting"
      )
task1_finegray$set_col_roles("fgwt", roles = "weight")
task1_finegray$select(c("x1", "x2"))
task1_finegray$set_col_roles("ssid", roles = "group")

   split = partition(task1, ratio
 
# Define a learner
learner <- lrn("surv.cv_glmnet", alpha = 0.1)

# Training the model on the Fine-Gray task
train_indices_fg <- seq_len(nrow(fg_data))  # Using all rows since it's a small dataset
learner$train(task1_finegray, row_ids = train_indices_fg)

# Generate predictions
prediction <- learner$predict(task1)
prediction

# survival probabiliy for selected time points
res = prediction$distr[1:6]$survival(c(1, 3, 5, 7))
round(res, 4)

# continuous ranking
prediction$crank[1:3]


