rm(list =ls())
seed  <- 2134
glmnet_args <- list(alpha = 0.5, nfolds = 5)
ranger_args <- list(num.trees = 500, mtry = 2, min.node.size = 3, seed=seed)

configure_learner <- function(learner_type, learner_args = list(), traceit = FALSE) {
  # Create the learner
  learner <- lrn(learner_type)
  
  # Print available parameters for the learner
  if (traceit) print(sort(learner$param_set$ids()))
  
  # Check and assign parameter values if provided
  if (length(learner_args) > 0) {
    valid_args <- intersect(names(learner_args), learner$param_set$ids())
    if (length(valid_args) != length(learner_args)) {
      warning("Some provided arguments are not valid for this learner: ", 
              paste(setdiff(names(learner_args), learner$param_set$ids()), collapse = ", "))
    }
    learner$param_set$values[names(learner_args)] <- learner_args
  }
  
  return(learner)
}

lrn0 = configure_learner("surv.cv_glmnet", list())
lrn1 = configure_learner("surv.cv_glmnet", glmnet_args)
lrn2 = configure_learner("surv.ranger", ranger_args)

lrn0$param_set$values
lrn1$param_set$values
lrn2$param_set$values