#---  Target vars `time` and `status` defined
#-    Note: event variable may have two representations (numeric and factor)
t_vars01   = c(time= "TIME_ESRD", event= c(num= "ESRD"), id='01')
t_vars02   = c(time= "TIME_LOSS50", event= c(num= "LOSS50"), id='02')
t_vars03   = c(time= "TIME_LOSS50_ESRD", event= c(num= "LOSS50_ESRD"), id='03')
t_vars04   = c(time= "TIME_ESRD4", event= c(num= "ESRD4"), id='04')
t_vars05   = c(time= "TIME_ESRD7", event= c(num= "ESRD7"), id='05')
t_vars06   = c(time= "TIME_ESRD10", event= c(num= "ESRD10"), id='06')
t_vars07   = c(time= "TIME_LOSS50_ESRD4", event= c(num= "LOSS50_ESRD4"), id='07')
t_vars08   = c(time= "TIME_LOSS50_ESRD7", event= c(num= "LOSS50_ESRD7"), id='08')
t_vars09   = c(time= "TIME_LOSS50_ESRD10", event= c(num= "LOSS50_ESRD10"), id='09')
t_vars10   = c(time= "TIME_DEATH", event= c(num= "DEAD"), id='10')
     
t_vars31 = c(time= "eTIME_ESRD",         event= c(num= "event_ESRDx",        fac = "event_ESRDf"),   id='31') # fac= needed
t_vars32 = c(time= "eTIME_LOSS50",       event= c(num= "event_LOSS50x",      fac=  "event_LOSS50f"), id='32')
t_vars33 = c(time= "eTIME_LOSS50_ESRD",  event= c(num= "event_LOSS50_ESRDx", fac= "event_LOSS50_ESRDf" ), id='33')
t_vars34 = c(time= "eTIME_ESRD4",        event= c(num= "event_ESRD4x",       fac= "event_ESRD4f" ), id='34')
t_vars35 = c(time= "eTIME_ESRD7",        event= c(num= "event_ESRD7x",       fac= "event_ESRD7f"),  id='35')
t_vars36 = c(time= "eTIME_ESRD10",       event= c(num= "event_ESRD10x",      fac= "event_ESRD10f"), id='36')
t_vars37 = c(time= "eTIME_LOSS50_ESRD4", event= c(num= "event_LOSS50_ESRD4x",fac= "event_LOSS50_ESRD4f" ), id='37')
t_vars38 = c(time= "eTIME_LOSS50_ESRD7", event= c(num= "event_LOSS50_ESRD7x",fac= "event_LOSS50_ESRD7f"),  id='38')
t_vars39 = c(time= "eTIME_LOSS50_ESRD10",event= c(num= "event_LOSS50_ESRD10x",fac="event_LOSS50_ESRD10f"), id='39')

# Create ``tvars_mtx` matrix with four columns:  time, event.num, event.fac,id  
 
tvar_nms1 = sprintf("t_vars%02d", 1:10)
tvar_nms3 = sprintf("t_vars%02d", 31:39)

# Use mget to get the list of vectors
vector_list1 <- mget(tvar_nms1)
vector_list3 <- mget(tvar_nms3)

# Create the matrix using do.call and rbind
tvars_mtx1 <- do.call(rbind, vector_list1)
tvars_mtx2 = cbind(tvars_mtx1, event.fac = "NA")
tvars_mtx3 <- do.call(rbind, vector_list3)
ord = colnames(tvars_mtx3)
tvars_mtx2_ord = tvars_mtx2[ , ord]
tvars_mtx0 =rbind(tvars_mtx2_ord, tvars_mtx3)
ord0 = c("id", "time","event.num", "event.fac")
tvars_mtx = tvars_mtx0[, ord0]

