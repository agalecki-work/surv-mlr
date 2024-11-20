predict_risk_at_time <- function(model, task, t_star) {
  target_nms = task$target_names
  nm1 =  target_nms[1]  # "time"
  time_pts = task$data()[[nm1]]
  # Get the unique time points
  time_points = sort(unique(time_pts))

  predictionsObject = model$predict(task)
 # This assumes predictions$surv is a matrix of survival function evaluations 
  pred_table = as.data.table(predictionsObject)
  pred_distr = pred_table$distr # List. one element(matrix  per subject
  surv_probs = pred_distr[[1]][[1]] # One matrix extractedd out of many
  #surv_probs = predictions$surv
 # Extract survival probabilities at time t_star
 # Note: Exact matching might require interpolation if t_star is not in predictions$times
  t_star_idx = which.min(abs(time_points - t_star))
  risk_at_t_star = 1 - surv_probs[, t_star_idx]
  return(risk_at_t_star)
}

#--- Using the function
# t_star = 90  # Example time point
# pred_table = as.data.table(learner$predict(task))
# names(pred_table) #  "row_ids" "time"    "status"  "crank"   "lp"      "distr"  
# pred_distr = pred_table$distr # list
# length(pred_distr)  # corresponds to 
# risk_scores = predict_risk_at_time(learner, task, t_star)
# print(risk_scores)