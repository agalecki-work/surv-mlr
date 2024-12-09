ctree_args          = list()
# cv_coxboost_args  = list()
mboost_args         = list()
svm_args            = list(gamma.mu =0.1)

# Vector with surv.learners

prj_learners_info    = c(
     # surv.cv_coxboost = "cv_coxboost_args", Error
     # surv.mboost = "mboost_args",
     surv.ctree  = "ctree_args", 
     surv.svm    = "svm_args"
     ) # 