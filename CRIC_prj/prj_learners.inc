# R code creates named vector `prj_learners_info` with info for `surv.learners` other than surv.cv_glmnet 
# 
ctree_args          = list()
# cv_coxboost_args  = list()
mboost_args         = list()
svm_args            = list(gamma.mu =0.1)

# Named vector with surv.learners

prj_learners_info    = c(
     # surv.cv_coxboost = "cv_coxboost_args", Error
     # surv.mboost = "mboost_args",
     surv.ctree  = "ctree_args", 
     surv.svm    = "svm_args"
     ) # 
     
# prj_learners_info = NULL
