library(survival)
library(glmnet)
library(mlr3)
library(mlr3proba)
library(rms)

# Assuming `original_data` is your dataset containing required columns (e.g., 'time', 'status', covariates)
#--- Data Splitting
# Splitting data into 70% training and 30% validation sets
set.seed(123)  # For reproducibility
train_idx <- sample(seq_len(nrow(original_data)), size = 0.7 * nrow(original_data))
train_data <- original_data[train_idx, ]
validation_data <- original_data[-train_idx, ]

# Checking proportions
cat("Number of training samples:", nrow(train_data), "\n")
cat("Number of validation samples:", nrow(validation_data), "\n")

#---Fine-Gray Transformation
# Apply finegray transformation to the training data
train_fg <- finegray(Surv(time, status) ~ ., data = train_data, etype = event_type)

#--- Convert training data to matrix for glmnet
train_matrix <- as.matrix(train_fg[, -which(names(train_fg) %in% c("time", "status", "fgstart", "fgstop", "fgstatus"))])
response <- Surv(train_fg$fgstart, train_fg$fgstop, as.numeric(train_fg$fgstatus))
weights <- train_fg$fgwt

#---Model Fitting with cv.glmnet
# Perform cross-validated Lasso for Cox model using cv.glmnet
cv_fit <- cv.glmnet(train_matrix, response, family = "cox", weights = weights, alpha = 1)
best_lambda <- cv_fit$lambda.min

#---Generate Linear Predictors for Validation Data
# Prepare validation data matrix (one row per subject)
validation_matrix <- as.matrix(validation_data[, -which(names(validation_data) %in% c("time", "status"))])
validation_lp <- predict(cv_fit, newx = validation_matrix, s = best_lambda, type = "link")
validation_data$lp <- as.numeric(validation_lp)

#---Evaluate Model Performance
# (a) Concordance Index (C-index)

#--- Fit a Cox model using the linear predictor from cv.glmnet as an offset
cox_model_validation <- coxph(Surv(time, status) ~ offset(lp), data = validation_data)
c_index <- summary(cox_model_validation)$concordance
print(c_index)

#(b) Calibration Curve

# Fit Cox model for calibration curve using the validation data
calibration_model <- cph(Surv(time, status) ~ lp, data = validation_data, x = TRUE, y = TRUE, surv = TRUE)

# Calibration plot
cal <- calibrate(calibration_model, method = "boot", B = 1000)
plot(cal)

#---- Summary
# Data Splitting: Split your dataset into training (70%) and validation (30%) sets.
# Fine-Gray Transformation: Transform the training data using the finegray function to handle competing risks.
# Model Fitting with cv.glmnet: Use cv.glmnet to find the optimal lambda for the Cox proportional hazards model with internal cross-validation.
# Generating Linear Predictors: Predict linear predictors for the validation set using the optimum lambda from cv.glmnet.
# Performance Evaluation: Evaluate the model's performance on the validation set using C-index and calibration plots.
