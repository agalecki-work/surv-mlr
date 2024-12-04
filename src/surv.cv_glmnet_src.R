 #~~~~~~

 # Use `feature_nms` and `Info$pen_factor_modify` to create `pen.fac` vector
     source("./src/create_penfactor.R") 
     
     txt = paste0("* -- Vector `pen.fac` with ", length(pen.fac), " elements created.")
     cat(txt, "\n")
     logx= c(logx, txt)

    # cat("--- `task1e` partitioned with ratio ", rat, " \n")

 
 alphas_nms = names(Info$alphas)
 
 
 surv.cva_glmnet_learners= vector(mode="list", length = length(alphas_nms))
 names(surv.cva_glmnet_learners) = alphas_nms 
 
 surv.cva_glmnet_prediction= vector(mode="list", length = length(alphas_nms))
 names(surv.cva_glmnet_prediction) = alphas_nms 
 
 
 cva_glmnet_fits = vector(mode="list", length = length(alphas_nms))
 names(cva_glmnet_fits) = alphas_nms
 
 if (length(event_lvls) == 2) pred1_scores = vector(mode="list", length = length(alphas_nms))

 cvglmnet_args_init = c(cvglmnet_args, list(penalty.factor = pen.fac, foldid=foldid))
 txt0 = paste(names(cvglmnet_args_init), collapse ="`, `")
 txt=paste0("* `cvglmnet_args_init` names (wout alpha): `", txt0, "`")  
 cat(txt, "\n")
 logx= c(logx, txt)
 
 #txt = paste0("* Learner `surv.cv_glmnet` for alpha named: ", anm, " for an event with two levels")

 
#=== FOR ai ==============
 for (ai in seq_along(Info$alphas)){
 # Define learner
    
    ax= Info$alphas[ai]
    learner_args_all = c(cvglmnet_args_init, list(alpha = ax))

    anm = alphas_nms[ai]

    txt = paste0("* --- Alpha =", ax, " processed")
    cat(txt, "\n")
    logx= c(logx, txt)

    if (length(event_lvls) == 2){

      learner = lrn("surv.cv_glmnet")
      learner$param_set$values[names(learner_args_all)] <- learner_args_all
      cat("--- Learner defined (type =`surv.cv_glmnet`, alpha=", ax, ") \n")
     
 
 # pass the task to the learner via $train()
 ### if (length(event_lvls) == 3) learner$train(task1e_FG, split$train) else learner$train(task1e, split$train) 
    learner$train(task1e, split$train)
    prediction = learner$predict(task1e, row_ids = split$test)   # prediction on test data 
   #  cat("--- prediction on test data \n")

   # inspect the trained model
   # coef(learner$model$model)
 
   #--- predict_type = "distr"

   prediction$distr[1:3]$survival(c(1, 3, 5, 7)) # prob surviving 5 years for the first 3 subjects in test data
   # cat("--- prediction survival \n")


#---- predict_type = "crank" stands for continuous ranking. 
#-  In mlr3proba there is one consistent interpretation of crank: 
# lower values represent a lower risk of the event taking place and higher values represent higher risk.
prediction$crank[1:3]
# cat("--- prediction crank \n")

## ==== MeasureSurv

as.data.table(mlr_measures)[
  task_type == "surv", c("key", "predict_type")][1:5]
  
# surv.rcll  RCLL (right-censored logloss) to evaluate the quality of distr predictions
# concordance index to evaluate a model’s discrimination,
# D-Calibration to evaluate a model’s calibration 
###--- if (length(event_lvls) == 2){
   score_ai = prediction$score(msrs(c("surv.graf", "surv.rcll", "surv.cindex", "surv.dcalib")))
} # if (length(event_lvls) == 2)

# Save results for learner `surv.cv_glmnet`
  tvarsx = Info_tvars$target_cols
  tvarsx_id = tvarsx["id"]
  tvarsx_tm = tvarsx["time"]
  
  tmp = learner_args_all$foldid 
  if (length(tmp) > 0) foldidx = paste0(":vector with ", length(tmp), " elements") 
  ### Info$foldid_cvglmnet = foldidx

   if (length(event_lvls) == 2){
      save_objects0 = c("surv.cva_glmnet_learners", "surv.cva_glmnet_prediction")
      surv.cva_glmnet_learners[[ai]] = learner
      surv.cva_glmnet_prediction[[ai]] = prediction
      pred1_scores[[ai]] = score_ai
      rm(learner, prediction)
     } else {
      cat("save3 \n")
      #cvfit <- cv.glmnet(X_train, y_train, family = "multinomial", alpha = 0.5)  # Elastic net
      train_data= as.data.table(task1e$filter(rows=split$train))
      yt = t_vars["event.fac"]
      y_train <- unlist(train_data[,yt, with=FALSE])
  
      xrm = c(t_vars["time"],t_vars["event.fac"])
      X_train <- as.matrix(train_data[, (xrm) :=NULL])
      learner_args_all$s =NULL
      learner_args_allmult =c(learner_args_all, list(family = "multinomial", x=X_train, y =y_train))
      cvfit = do.call("cv.glmnet", learner_args_allmult)
      cva_glmnet_fits[[ai]] = cvfit
      save_objects0 = c("cva_glmnet_fits")
    }
#--- Cleanup


} # for (ai in seq_along(Info$alphas)
#=== END FOR ai

#--- Results for all alphas
  if (length(event_lvls) == 2){    
     cv_fits = lapply(alphas_nms,  FUN= function(a){
      x=surv.cva_glmnet_learners[[a]]
      x$model$model})
      #--- pred_scoreAll
      pred1_scoreList = lapply(pred1_scores, FUN = function(x) as_tibble(t(x)))
      names(pred1_scoreList) = alphas_nms
      pred_scoreAll = bind_rows( pred1_scoreList, .id = "alpha_lbl")
      pred_scoreAll["learner_lbl"] = "surv.cv_glmnet"


      
    } else cv_fits = cva_glmnet_fits
  
  #--- glanceAll
     glanceList =lapply(cv_fits, myglance)
     names(glanceList) = alphas_nms
     glanceAll =bind_rows(glanceList, .id = "alpha_lbl")
     
     glance_melt = glanceAll %>% select(alpha_lbl, index_min, index_1se) %>%
       gather(index, step,  -alpha_lbl)
       
  #--- tidyAll
     tidyList = lapply(cv_fits, mytidy)
     names(tidyList) = alphas_nms
     tidyAll0 = bind_rows(tidyList, .id = "alpha_lbl") 
     
     tidyAll = tidyAll0 %>% left_join(glance_melt, by= c("alpha_lbl","step"))  %>%
               mutate( index = case_when(
                 is.na(index) ~ "",
                 .default = as.character(index)
                 ))
  
  if (length(event_lvls) == 2){    
  
    #--- coefAll  Beta coefficients
     coef_nms = rownames(coef(cv_fits[[1]]))
     coefList = lapply(cv_fits, FUN= function(x){
        vecx = as.matrix(coef(x))
        dim(vecx) =NULL
        vecx
        })
      coefAll = cbind(coef_nms, bind_cols(coefList))
      names(coefAll) = c("coef", alphas_nms)
    #  coef_alldot <- coefAll %>%  mutate(across(where(is.numeric), ~ round(.,4))) %>%
    #  mutate(across(everything(), ~ ifelse(. == 0, ".", as.character(.))))
  } else {
  
    multi_nms = names(coef(cv_fits[[1]])) #
    
    x1 = cv_fits[[1]]  # class(x1)= cv.glmnet
    coefMtx1 = x1$glmnet$beta #list with three components
    coefMtx =  coefMtx1[[2]] # 
    coef_nms = rownames(coefMtx)
    coefList = lapply(cv_fits, FUN= function(x){
          #cat("??? \n ")
          #print(class(x))
          #cat("???1 \n ")
          xfit = x$glmnet.fit
          #cat("???2a \n ")
          coef3 = xfit$beta # list with 3 components:  "censor" "ESRD"   "death"
          #cat("???3 \n ")
 
          ## coef_nms = rownames(coef3[[2]]) # vector
          coefs1 = coef3[2]    # list with one component:ESRD, contains matrix with cols s0, s1... 
          coefsMtx = coefs1[[1]]  # 19 x 80 sparse Matrix of class "dgCMatrix"
          lambda_min = x$lambda.min
          idx_min    = x$index["min","Lambda"]
          coefs = coefsMtx[, idx_min]  # select column   
          coefs
          #vecx = as.matrix(coefs)
          #dim(vecx) =NULL
	  #vecx
          })
         coefAll = cbind(coef_nms, bind_cols(coefList))
         names(coefAll) = c("coef", alphas_nms)
  }
      
  save_objects = c("data1e", "anl_summary","logx", "prj_Info", "Info", save_objects0) 
  
  fpath0 = paste0(prj_path,"/", anl_name, "/", tvarsx_id , tvarsx_tm)
  fpath = paste0(fpath0, "_cva_glmnet.Rdata") 
  save(list =save_objects, file =fpath)
  
  txt0 = paste(save_objects, collapse=", ")
  txt = paste0("* Objects: ", txt0, " saved in '", fpath) #_cva_glmnet.Rdata 
  cat(txt, "\n") 
  logx = c(logx, txt)
  
  #-- xlsx
  ## fpath1 =paste0(prj_path,"/", anl_name, "/_prj_info.xlsx")
  wb = createWorkbook()
  addWorksheet(wb, "Anl_summ")
  writeData(wb, "Anl_summ", anl_summary)  
  addWorksheet(wb, "glanceAll")
  writeData(wb, "glanceAll", glanceAll)
  addWorksheet(wb, "tidyAll")
  writeData(wb, "tidyAll", tidyAll)
  addWorksheet(wb, "coefAll")
  writeData(wb, "coefAll", coefAll)
  ########## fpathx =paste0(fpath0, "_cva_glmnet.xlsx")
  

  txt = paste0("* Multiple WorkSheets added to Excel Workbook")
  cat(txt, "\n") 
  logx = c(logx, txt)

# ~~~~~