
funNms <- c(
  "S05-select_columns_data",
  "S10-apply_admin_censoring",
  "S15-mice_impute_data",
  "S20-create_surv_task',
  "check_best_params",
  "predict_risk_at_time",
  "expand_factors2dummy",
  "run_pipeline" 
)

funNmsR  <- paste0(funNms,".R") 
lapply(funNmsR, FUN= function(src) source(paste0( "./R/", src)))

rm(funNmsR)
