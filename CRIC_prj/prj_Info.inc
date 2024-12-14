# ==== `prj_Info` describes input files
# 
# `original nms:  Names of original data stored in SAS format and in external Rdata
# `complete nme:  Name of complete data data `df_complete' stored in external Rdata
# `add_vars_name: R code used to create/add new variables to `df_complete`
# `prj_learners:   R code that creates  named vector `prj_learners_info` with surv.learners info
# Note: Input files mentioned in this document are stored in CRIC_prj or in CRIC_prj/data folders

prj_Info = list(
  original_nms     = c(df= "cric_olinknpx_112023_v1_sas7bdat", Rdata= "cric_olinknpx_112023_v1.Rdata"),
  complete_nms     = c(df= "df_complete", Rdata= "02-cric_complete112023.Rdata"),
  add_vars_name    = "cric_add_variables.inc",
  tvars_mtx_name   = "cric_tvars_mtx.inc"    ,
  prj_learners     = "prj_learners.inc" ,
  dateStamp        = "_map.log"           #  File in a folder with analysis results
)  

  