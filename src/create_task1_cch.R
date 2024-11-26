 #--- Ensure that split into test and train samples is stratified by subcohort 0/1 variable
 # Executed if CCH  ==TRUE ( i.e. `dcol_nms` vector contains element named subcohort)
 # `task1` created
 # Input: Info$dcol_nms, df_complete
 
 # Output: 
 #   intermediate: task_cch0, task_cch1 (appended)
 #   final: task1, split
 
 ext_01 = Info$d_cols["external_01"]
 
 rat = Info$partition_ratio
 txt =paste0("* Sourcing `.src/create_task1_cch.R`. `task1` created for CCH data. CCH=", CCH, ", rat=", rat)
 cat(txt, "\n")
 logx = c(logx, txt)
 
 subcch_nm = dcol_nms["subcohort"]
 subcch_vec = unlist(df_complete[, ..subcch_nm])
 df_complete = df_complete[order(subcch_vec), ]  # df_complete sorted by subcohort
 set.seed(Info$seed["split"])
  
 # Stratum outside of subcohort ( ==0)

 cstmnt0 = paste0(subcch_nm, "==0")
 df_cch0 = subset(df_complete, subset= eval(parse(text= cstmnt0)))
 
 if (length(event_lvls) == 2){          # type = "right" by  default
        df_cch0_num = df_cch0           #  `df_cch0` contains both event.num and event.com
 	df_cch0_num[[evnt_fac]] = NULL  #  Factor event column dropped, numeric event kept
  	df_cch0_num[[ext_01]] = NULL
        task_cch0 = TaskSurv$new(id = "cch0_num", backend = df_cch0_num, 
                              time = time_nm, event = evnt_num)
 } else {
        #----- TaskSurv with type=mstate
        df_cch0_fac = df_cch0           #  `df_cch0` contains both event.num and event.com
        df_cch0_fac[[evnt_num]] = NULL  #  Num event column dropped, factor event kept
  	df_cch0_fac[[ext_01]] = NULL
 
        task_cch0 = TaskSurv$new(id = "cch0_fac", backend = df_cch0_fac, 
                              time = time_nm, event = evnt_fac,
                              type = "mstate")
 } # length(event_lvls) == 2)
 
 task_cch0 = accept_col_roles(task_cch0, Info)

 # Split `task_cch0`
     set.seed(Info$seed["foldid"])
     if (!is.na(rat)){
       split0 = mlr3::partition(task_cch0, ratio = rat)
       train_cch0_indices = split0$train
       test_cch0_indices = split0$test
       
     } else {
       if (!(ext_01 %in% names(df_complete))) stop("rat=NA, variable `", ext_01,"` not found")
       ord = 1:nrow(df_cch0)
       tmp1 = df_cch0[, ..ext_01] ==1
       train_cch0_indices = ord[tmp1]
       tmp0 = df_cch0[, ..ext_01] ==0
       test_cch0_indices  = ord[tmp0]
     }
       foldid_cch0 = sample(1:nfolds, length(train_cch0_indices), replace=TRUE)


 # Stratum inside subcohort (== 1)
 
  cstmnt1 = paste0(subcch_nm, "==1")
  df_cch1 = subset(df_complete, subset= eval(parse(text= cstmnt1))) # 
  
 # 
 if (length(event_lvls) == 2){  #--- task_cch1
        df_cch1_num = df_cch1           #  contains both event.num and event.fac
 	df_cch1_num[[evnt_fac]] = NULL  #  Factor event column dropped, numeric event kept
  	df_cch1_num[[ext_01]] = NULL 
        task_cch1 = TaskSurv$new(id = "cch1_num", backend = df_cch1_num, 
                              time = time_nm, event = evnt_num)
 } else {
        #----- TaskSurv with type=mstate
        df_cch1_fac = df_cch1           #  contains both event.num and event.fac
        df_cch1_fac[[evnt_num]] = NULL  #  Num event column dropped, factor event kept
        df_cch1_fac[[ext_01]] = NULL
        task_cch1 = TaskSurv$new(id = "cch1_fac", backend = df_cch1_fac, 
                              time = time_nm, event = evnt_fac,
                              type = "mstate")
 } # length(event_lvls) == 2)
 
 task_cch1 = accept_col_roles(task_cch1, Info)

 # Split `task_cch1`
     set.seed(Info$seed["foldid"])

     rat = Info$partition_ratio
     if (!is.na(rat)){
      split1 = mlr3::partition(task_cch1, ratio = rat)
      train_cch1_indices = split1$train
      test_cch1_indices = split1$test
     } else {
       if (!(ext_01 %in% names(df_complete))) stop("rat=NA, variable `", ext_01,"` not found")
       ord = 1:nrow(df_cch1)
       tmp1 = df_cch1[, ..ext_01] ==1
       train_cch1_indices = ord[tmp1]
       tmp0 = df_cch1[, ..ext_01] ==0
       test_cch1_indices  = ord[tmp0]
     }

     foldid_cch1 = sample(1:nfolds, length(train_cch1_indices), replace=TRUE)
      train_cch1_indices_adj =  train_cch1_indices + nrow(df_cch0)
      test_cch1_indices_adj  =  test_cch1_indices + nrow(df_cch0)

    
    
#--- Append task_cch0 and task_cch1

if (length(event_lvls) == 2){
   df_task1num =rbind(df_cch0_num, df_cch1_num)
   task1 = TaskSurv$new(id = "cch_num combined(type=right)", backend = df_task1num, 
                                 time = time_nm, event = evnt_num)
} else {
   df_task1fac =rbind(df_cch0_fac, df_cch1_fac)
   task1 = TaskSurv$new(id = "cch_fac combined(mstate=3)", backend = df_task1fac, 
                                 time = time_nm, event = evnt_fac,
                                 type = "mstate")
}

task1 = accept_col_roles(task1, Info)

  train_indices = c(train_cch0_indices, train_cch1_indices_adj)
  test_indices = c(test_cch0_indices, test_cch1_indices_adj)
 
  foldid =c(foldid_cch0,foldid_cch1)
  split = list(train = train_indices, test= test_indices, validation= integer(0))
  rm(train_indices, test_indices, train_cch0_indices, train_cch1_indices_adj, test_cch0_indices, test_cch1_indices_adj)
  