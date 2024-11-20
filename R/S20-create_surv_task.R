
create_surv_task <- function(data, target_time, target_status) {
  task = TaskSurv$new(id = paste0("task_", target_time), backend = data, 
                           time = target_time, event = target_status)
  task$set_col_roles(c("id",         "weights"), 
                     c("identifier", "weight"))
  return(task)
}
