accept_col_roles = function(task, Info){
  # Accepts roles for id_nm, wght_nm and start_nm
  id_nm = Info$d_cols["id"]
  task$set_col_roles(id_nm, roles = "group")
  
  wght_nm = Info$d_cols["weight"]
  if (length(wght_nm) > 0 && !is.na(wght_nm)) task$set_col_roles(wght_nm, roles = "weight")
  
  #--- Error: Cannot combine stratification with grouping
  #strat_nm = Info$d_cols["stratum_split"]
  #if (length(strat_nm) > 0 && !is.na(strat_nm)) task$set_col_roles(strat_nm, roles = "stratum")
  return(task)
}

