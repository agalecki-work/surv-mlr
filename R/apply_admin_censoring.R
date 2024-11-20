
apply_admin_censoring <- function(data, time_col, event_col, censor_time = Inf) {
 # time_col  character
 # event_col character
    censor_timex = if (is.infinite(censor_time)) max(time_col) else censor_time 
    setDT(data)  # Ensure the data is a data.table
    data[, (event_col) := fifelse(get(time_col) > censor_time, 0, get(event_col))]
    data[, (time_col)   := pmin(get(time_col), censor_timex)]
    return(data)
}
