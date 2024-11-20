# Administrative censoring function
apply_admin_censoring <- function(data, time_col, status_col, censor_time) {
  # requires data.table
  setDT(data)  # Ensure the data is a data.table
  data[, (time_col) := pmin(get(time_col), censor_time)]
  data[, (status_col) := ifelse(get(time_col) < censor_time, get(status_col), 0)]
  return(data)
}

# lung = survival::lung
lungx = within(survival::lung, {
     status1  = as.numeric(status)
     rm(status)
    })
datax = apply_admin_censoring(lungx, "time", "status1", 100) 
