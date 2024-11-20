

#----- TaskSurv 


if (length(event_lvls) == 2){ 
        df_complete_num = df_complete       # `df_complete` contains both event.num and event.com
	df_complete_num[[evnt_fac]] = NULL  #  Factor event column dropped, numeric event kept

        task1 = TaskSurv$new(id = paste0("task1_", Info$analysis_lbl  ), backend = df_complete_num, 
                             time = time_nm, event = evnt_num)
} else {
       #----- TaskSurv with type=mstate
       df_complete_fac = df_complete       # `df_complete` contains both event.num and event.com
       df_complete_fac[[evnt_num]] = NULL  #  Num event column dropped, factor event kept

        task1 = TaskSurv$new(id = paste0("task1_"  ), backend = df_complete_fac, 
                             time = time_nm, event = evnt_fac,
                             type = "mstate")
} # length(event_lvls) == 2)



# Syntax: Task$set_col_roles(cols, roles = NULL, add_to = NULL, remove_from = NULL)
 
task1 = accept_col_roles(task1, Info)

# Split `task1`
    set.seed(Info$seed["split"])
    rat = Info$partition_ratio
    split = mlr3::partition(task1, ratio = rat)
    train_indices = split$train
    test_indices = split$test
    set.seed(Info$seed["foldid"])
    foldid = sample(1:nfolds, length(train_indices), replace=TRUE)


