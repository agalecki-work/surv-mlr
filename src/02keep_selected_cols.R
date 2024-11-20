#====  Keep selected columns in `df_complete` data frame

keep_vars0 = unique(c(Info_tvars$target_cols[c("time", "event.num", "event.fac")],  Info$feature_cols, Info$d_cols, Info$add_cols))
keep_vars = keep_vars0[!is.na(keep_vars0)]

diffx = setdiff(keep_vars, names(df_complete)) # It should return character(0)
if (length(diffx) > 0){
   cat(" ERROR. Vars ", diffx, " not found \n" ) 
  } else {
   cat ("  ... OK \n")
}
df_complete = df_complete[, keep_vars]
