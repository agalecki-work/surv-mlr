# https://stackoverflow.com/questions/60620158/using-mlr3-pipelines-to-impute-data-and-encode-factor-columns-in-graphlearner


library(mlr3) 
library(mlr3pipelines)
library(mlr3learners)
library(mlr3tuning)
library(paradox)

task <- tsk("pima")
task$missings()

#---- create factor

hb <- po("histbin", param_vals =list(affect_columns = selector_name("triceps")))

# impute new level and encode:

imp_cat <- po("imputenewlvl",
              param_vals =list(affect_columns = selector_name("triceps")))
encode <- po("encode",
             param_vals = list( affect_columns = selector_name("triceps")))

cat <- hb %>>% 
  imp_cat %>>%
  encode