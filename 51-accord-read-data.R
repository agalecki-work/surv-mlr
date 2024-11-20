# source("51-accord-read-data.R")
rm(list = ls())

library(dplyr)
fpath = "./data/olink_analytical_dataset011123.Rdata"

load(fpath,verbose=TRUE)
accord0 = olink_analytical_dataset011123
cat(" Dim accord0:", dim(accord0), "/n")

# remove invalid observations
accord = accord0 %>% filter(YRS_PRIMARY >0) 

cat(" Nrow accord:", nrow(accord), "/n")

nms_all= names(accord)

bmq_vars = paste("BMQ_", 1:21, sep="")

bm_vars = paste("BM", 1:21, sep="")
ccn_vars = paste("CCN", 1:7, sep="")

id_vars =c("MASKID", "BSI_ID", "PLATE")
dx_vars =c("BPTRIAL", "BPINT", "GLYINT", "FIBRATE") 

nms1 = setdiff(nms_all, c(bm_vars, bmq_vars, ccn_vars, id_vars,dx_vars))
nms1
t1_vars = c("YRS_PRIMARY", "PRIMARY")
t2_vars = c("YRS_CASE40_JUN", "CASE40_JUNE")
t3_vars = c("YRS_DECLINE40_PLUS", "DECLINE40_PLUS")
t4_vars = c("FU_TM_ACCORDION", "TM_ACCORDION")
tvars_mtx2 = matrix(c(t1_vars, t2_vars, t3_vars, t4_vars), 
                ncol=2, byrow=TRUE)
colnames(tvars_mtx2) = c("time", "event")


tvars_all2 = tvars_mtx2 # vector
dim(tvars_all2) = NULL
nms2 = setdiff(nms1, tvars_all2)

cat("nms_all:", length(nms_all))
cat("dx_vars:", length(dx_vars))

