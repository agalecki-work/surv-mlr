
### Expanded task1 

### Mutate 
task1e = pom$train(task1)$mutate.output

# save data (no case-cohort)

dtx0 = as.data.table(task1e)
dtcold = df_complete[, ..dcol_nms]
train_ind  = rep(0, nrow(df_complete))
train_ind[split$train_indices] = 1
data1e = cbind(dtcold, train_ind, dtx0) # will be saved





                            
