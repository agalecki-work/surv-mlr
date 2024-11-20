# source("test_admin_censoring.R")

rm(list=ls())

apply_admin_censoring <- function(data,id_col, time_col, status_col, censor_time=Inf, traceit = FALSE) {
# Administrative censoring function
  if (!is.finite(censor_time)) censor_time = max(data[[time_col]])*1.1
  
  if (traceit) message("==== apply_admin_censoring() STARTS")
  setDT(data)  # Ensure the data is a data.table

  data[, (status_col) := ifelse(get(time_col) > censor_time, 0, get(status_col))]
  data[, (time_col) := pmin(get(time_col), censor_time)]

  if (traceit) {
    message("-- apply_admin_censoring() executed")
    #print(str(data))
    #print(table(data[[status_col]])) 
    message("---- apply_admin_censoring() ENDS")
  }
  return(data)
}


################### Execution starts here ###########


# Load necessary libraries
library(mlr3)
library(mlr3proba)
library(mlr3learners)
library(data.table)
library(mice)
library(survival)

# Load mgus2 dataset and add id, weights column
mgusx <- as.data.table(survival::mgus2)
mgusx[, `:=`(id = .I)]  # Add an ID column
set.seed(123)  # Setting seed for reproducibility
mgusx[, wght := runif(.N, 0.5, 1.5)]  # Add a weights column

etime <- with(mgusx, ifelse(pstat == 0, futime, ptime))
event <- with(mgusx, ifelse(pstat == 0, 2 * death, 1))  # Numeric with values 0, 1, 2
## event <- factor(event, 0:2, labels = c("censor", "pcm", "death"))
mgusx[, `:=`(etime = etime, event = event)]


message("====> Input info data mgusx n=", nrow(mgusx))

time_cols   = c("futime", "etime")
status_cols = c("pstat", "event")
sel  = 2   # or 2     <----
time_col   = time_cols[sel]
status_col = status_cols[sel] 
censor_time = 300
statusx = mgusx[[status_col]] # numeric vector
lvls = unique(statusx)        # ... with values 0,1,2
timex   = mgusx[[time_col]]
message("Time col: ", time_col, ". Max value;", max(timex))
message(status_col, " is a vector of mode ", mode(statusx), " with ", length(lvls), " unique values")
print(table(statusx))



dtout <- apply_admin_censoring(
          data= mgusx, 
          id_col = "id", 
          time_col = time_col,
          status_col = status_col, 
          censor_time = censor_time, 
          traceit=TRUE)
message("---- Output info dtout n= ", nrow(dtout))          
# print(colnames(dtout))
print(table(dtout[[status_col]]))