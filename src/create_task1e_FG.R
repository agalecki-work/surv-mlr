
#==== Prepare data for finegray

df_fgin = as.data.table(task1e)  # event factor included, factors expanded
df_fgin[[nm_id]] = df_complete[[nm_id]]

df_expanded_FG <- finegray(Surv(get(time_nm), event = event_fac) ~ ., data = df_fgin,
  etype = event_lvls[2], id= get(nm_id))


rnms = c(Info$target_cols, Info$d_cols["weight"], "(weight)") 
rnms = rnms[!is.na(rnms)]
df_expanded_FG[rnms] = NULL
df_expanded_FG = as.data.table(df_expanded_FG)

task1e_FG = TaskSurv$new(id = paste0("task1e_FG", Info$analysis_lbl), backend = df_expanded_FG, 
                             time = "fgstart", time2 = "fgstop",  event = "fgstatus",
                             type = "counting")

# Ensure the ID is a key for correct referencing
task1e_FG$set_col_roles(nm_id, roles = "group")
task1e_FG$set_col_roles("fgwt", roles = "weight")

#  Split the Fine-Gray task based on the original training/testing indices
train_ids <-  df_fgin[train_indices, ..nm_id]
test_ids  <-  df_fgin[test_indices,  ..nm_id]

cat("====1 ")
# Find corresponding rows in Fine-Gray data
train_indices_fg <- which(df_expanded_FG[[nm_id]] %in% unlist(train_ids))
test_indices_fg <- which(df_expanded_FG[[nm_id]] %in% unlist(test_ids))
cat("====2 ")
                              
