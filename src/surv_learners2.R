 surv_learners= vector(mode="list", length = length(learner_nms))
 names(surv_learners) = learner_nms 
 
 surv_prediction= vector(mode="list", length = length(learner_nms))
 names(surv_prediction) = learner_nms
 
 pred2_scores = vector(mode="list", length = length(learner_nms))
 names(pred2_scores) = learner_nms

 for (i in seq_along(learners_info)){
   lrn_type = learner_nms[i] # Ex. "surv.ctree"
   lrn_name = sub("surv.", "",  lrn_type) # "ctree"
   txt = paste0("* Learner: ", lrn_name, " processed")
   cat(txt, "\n")
   logx=c(logx, txt)
   
   learner = lrn(lrn_type)
   learner_args = get(learners_info[i])
   learner$param_set$values[names(learner_args)] <- learner_args
   learner$train(task1e, split$train)
   prediction = learner$predict(task1e, row_ids = split$test)
   score_i    = prediction$score(msrs(c("surv.graf", "surv.rcll", "surv.cindex", "surv.dcalib")))
   save_objects0 = c("surv_learners", "surv_prediction")
   surv_learners[[i]] = learner
   surv_prediction[[i]] = prediction
   pred2_scores[[i]] = score_i
   # cat("--- ", lrn_type, "processed ... \n") 
 } #  for i in seq_along(learners_info))
 

     pred2_scoreList = lapply(pred2_scores, FUN = function(x) as_tibble(t(x)))
     names(pred2_scoreList) = learner_nms
     pred2_scoreAll = bind_rows( pred2_scoreList, .id = "learner_lbl")
     pred2_scoreAll["alpha_lbl"] = ""
     pred_scoreAll = rbind(pred_scoreAll, pred2_scoreAll) 
    
    save_objects = c("prj_Info", "Info","logx",save_objects0)
     fpath = paste0(prj_path,"/", anl_name, "/", tvarsx_id , tvarsx_tm,  "_varia.Rdata")
     save(list =save_objects, file =fpath)
 
     txt0 = paste(save_objects, collapse=", ")
     txt  = paste0("* Objects: `", txt0, "` saved_in `", fpath, "`")
     cat(txt, "\n")
     logx = c(logx, txt)
 
     pred_scoreAll =  pred_scoreAll %>% relocate(learner_lbl, .before = alpha_lbl)
     addWorksheet(wb, "pred_scoreAll")
     writeData(wb, "pred_scoreAll", pred_scoreAll)
     
     txt = paste0("* pred_scoreAll workSheet added to xlsx")
     cat(txt, "\n")
     logx = c(logx, txt)
      