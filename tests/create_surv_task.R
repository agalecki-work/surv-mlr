create_surv_task <- function(data, time_col, time2_col, status_col, weight_col = NULL, type = "right", traceit = TRUE) {
  if (traceit) message("==== create_surv_task() STARTS: time=", time_col,", time2 =", time2_col, ", ", status_col, ", ", weight_col, ", type=", type)
  print(table(data[[status_col]]))
 print(colnames(data))
  task <- TaskSurv$new(id = paste0("task_", time_col), backend = data, time = time_col, event = status_col, time2 = time2_col, type = type)
  if (!is.null(weight_col)) task$set_col_roles(weight_col, "weight")

  if (traceit) message("==== create_surv_task() ENDS")

  return(task)
}
