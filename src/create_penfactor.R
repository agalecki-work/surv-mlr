  #-- create `pen.fac` vector ( using  feature_nms and Info$pen_factor_modify)
  # Input: feature_nms, Info$pen_factor_modify
    pen.fac = rep(1, length(feature_nms))
    names(pen.fac) = feature_nms
    # grep("^ACRcat.", names(pen.fac))
    ## Info$pen_factor_modify = c(BMI = 0.3, CKD. = 0)
    mod_vals = Info$pen_factor_modify
    mod_nms = names(mod_vals)
    mod_log = endsWith(mod_nms, ".") #logical vector
    mod1_nms = mod_nms[mod_log == FALSE] # exact name matches
    pen.fac[mod1_nms] = mod_vals[mod1_nms] 
    mod2_nms = mod_nms[mod_log == TRUE]  # ends with dot
   
    if (length(mod2_nms) > 0){
     for ( i in seq_along(mod2_nms)){
      pat = paste0("^", mod2_nms[i])
      valx = mod_vals[mod2_nms[i]]
      idx = grep(pat, names(pen.fac))
      pen.fac[idx] = valx
  }}
