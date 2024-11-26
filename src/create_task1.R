

#----- TaskSurv 
ext_01 = Info$d_cols["external_01"]

if (length(event_lvls) == 2){ 
        df_complete_num = df_complete       # `df_complete` contains both event.num and event.com
	df_complete_num[[evnt_fac]] = NULL  #  Factor event column dropped, numeric event kept
        df_complete_num[[ext_01]]   = NULL
        task1 = TaskSurv$new(id = paste0("task1_", Info$analysis_lbl  ), backend = df_complete_num, 
                             time = time_nm, event = evnt_num)
} else {
       #----- TaskSurv with type=mstate
       df_complete_fac = df_complete       # `df_complete` contains both event.num and event.com
       df_complete_fac[[evnt_num]] = NULL  #  Num event column dropped, factor event kept
       df_complete_fac[[ext_01]]   = NULL
       task1 = TaskSurv$new(id = paste0("task1_"  ), backend = df_complete_fac, 
                             time = time_nm, event = evnt_fac,
                             type = "mstate")
} # length(event_lvls) == 2)



# Syntax: Task$set_col_roles(cols, roles = NULL, add_to = NULL, remove_from = NULL)
 
task1 = accept_col_roles(task1, Info)


# Split `task1`
    rat = Info$partition_ratio
    if (!is.na(rat)){
     set.seed(Info$seed["split"])
     split = mlr3::partition(task1, ratio = rat)
    } else {
     
     if (!(ext_01 %in% names(df_complete))) stop("rat=NA, variable `", ext_01,"` not found")
     ord = 1:nrow(df_complete)
     tmp1 = df_complete[, ..ext_01] ==1
     train_indices = ord[tmp1]
     tmp0 = df_complete[, ..ext_01] ==0
     test_indices  = ord[tmp0]
     split=list(train = train_indices, test= test_indices, validation= integer(0))
     rm(train_indices, test_indices)
  }
     set.seed(Info$seed["foldid"])
     foldid = sample(1:nfolds, length(split$train), replace=TRUE)

