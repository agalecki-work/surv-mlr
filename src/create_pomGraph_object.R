## Define a pipeline with one-hot encoding preprocessing step using mlr3pipelines

po_onehot_fac = po("encode", method = "one-hot", affect_columns = selector_type("factor"))

apply_splines = function(x) as.data.table(splines::ns(x, df = 3))

if (is.null(Info$splined_vars)){
  pom = po_onehot_fac %>>% po("mutate")    
} else {
  pom =  po("colapply", 
         id = "spline_all", 
         applicator = apply_splines, 
         affect_columns = selector_name(Info$splined_vars))  %>>% 
         po_onehot_fac %>>% po("mutate")    
}

  