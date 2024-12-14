# Add/modify  variables  `df_complete`
# Input: `df_complete`
# Output: `df_complete` with vars added


# CRIC `df_complete`: 

#-- 3#: Create eTIME_??, event_??x and event_??f variables
#-- Template
#eTIME_??       <- with(df_complete, ifelse(??==0, TIME_DEATH, TIME_??))
#event_??x      <- with(df_complete, ifelse(?? ==0, 2*DEAD, 1)) # numeric with 0,1,2 values
#event_LOSS50f  <- factor(event_??x, 0:2, labels=c("censor", "??", "death"))

#df_complete$eTIME_?? = eTIME_??
#df_complete$event_??x = event_??x   # numeric 0:2
#df_complete$event_??f = event_??f   # factor

#-- 31: Create eTIME_ESRD, event_ESRDx and event_ESRDf
eTIME_ESRD  <- with(df_complete, ifelse(ESRD==0, TIME_DEATH, TIME_ESRD))
event_ESRDx <- with(df_complete, ifelse(ESRD==0, 2*DEAD, 1)) # numeric with 0,1,2 values
event_ESRDf <- factor(event_ESRDx, 0:2, labels=c("censor", "ESRD", "death"))



df_complete$eTIME_ESRD = eTIME_ESRD
df_complete$event_ESRDx = event_ESRDx   # numeric 0:2
df_complete$event_ESRDf = event_ESRDf   # factor
 
#-- 32: Create eTIME_LOSS50, event_LOSS50x and event_LOSS50f
eTIME_LOSS50   <- with(df_complete, ifelse(LOSS50==0, TIME_DEATH, TIME_LOSS50))
event_LOSS50x  <- with(df_complete, ifelse(LOSS50==0, 2*DEAD, 1)) # numeric with 0,1,2 values
event_LOSS50f <- factor(event_LOSS50x, 0:2, labels=c("censor", "LOSS50", "death"))

df_complete$eTIME_LOSS50 = eTIME_LOSS50
df_complete$event_LOSS50x = event_LOSS50x   # numeric 0:2
df_complete$event_LOSS50f = event_LOSS50f   # factor

#-- 33: Create eTIME_LOSS50_ESRD, event_LOSS50_ESRDx and event_LOSS50_ESRDf
eTIME_LOSS50_ESRD       <- with(df_complete, ifelse(LOSS50_ESRD==0, TIME_DEATH, TIME_LOSS50_ESRD))
event_LOSS50_ESRDx      <- with(df_complete, ifelse(LOSS50_ESRD ==0, 2*DEAD, 1)) # numeric with 0,1,2 values
event_LOSS50_ESRDf  <- factor(event_LOSS50_ESRDx, 0:2, labels=c("censor", "LOSS50_ESRD", "death"))

df_complete$eTIME_LOSS50_ESRD = eTIME_LOSS50_ESRD
df_complete$event_LOSS50_ESRDx = event_LOSS50_ESRDx   # numeric 0:2
df_complete$event_LOSS50_ESRDf = event_LOSS50_ESRDf   # factor

#-- 34: Create eTIME_ESRD4, event_ESRD4x and event_ESRD4f
eTIME_ESRD4       <- with(df_complete, ifelse(ESRD4==0, TIME_DEATH, TIME_ESRD4))
event_ESRD4x      <- with(df_complete, ifelse(ESRD4 ==0, 2*DEAD, 1)) # numeric with 0,1,2 values
event_ESRD4f      <- factor(event_ESRD4x, 0:2, labels=c("censor", "ESRD4", "death"))

df_complete$eTIME_ESRD4 = eTIME_ESRD4
df_complete$event_ESRD4x = event_ESRD4x   # numeric 0:2
df_complete$event_ESRD4f = event_ESRD4f   # factor

#-- 35: Create eTIME_ESRD7, event_ESRD7x and event_ESRD7f
eTIME_ESRD7       <- with(df_complete, ifelse(ESRD7==0, TIME_DEATH, TIME_ESRD7))
event_ESRD7x      <- with(df_complete, ifelse(ESRD7 ==0, 2*DEAD, 1)) # numeric with 0,1,2 values
event_ESRD7f      <- factor(event_ESRD7x, 0:2, labels=c("censor", "ESRD7", "death"))

df_complete$eTIME_ESRD7  = eTIME_ESRD7
df_complete$event_ESRD7x = event_ESRD7x   # numeric 0:2
df_complete$event_ESRD7f = event_ESRD7f   # factor

#-- 36: Create eTIME_ESRD10, event_ESRD10x and event_ESRD10f
eTIME_ESRD10       <- with(df_complete, ifelse(ESRD10==0, TIME_DEATH, TIME_ESRD10))
event_ESRD10x      <- with(df_complete, ifelse(ESRD10 ==0, 2*DEAD, 1)) # numeric with 0,1,2 values
event_ESRD10f      <- factor(event_ESRD10x, 0:2, labels=c("censor", "ESRD10", "death"))

df_complete$eTIME_ESRD10  = eTIME_ESRD10
df_complete$event_ESRD10x = event_ESRD10x   # numeric 0:2
df_complete$event_ESRD10f = event_ESRD10f   # factor

#-- 37: Create eTIME_LOSS50_ESRD4, event_LOSS50_ESRD4x and event_LOSS50_ESRD4f
eTIME_LOSS50_ESRD4       <- with(df_complete, ifelse(LOSS50_ESRD4==0, TIME_DEATH, TIME_LOSS50_ESRD4))
event_LOSS50_ESRD4x      <- with(df_complete, ifelse(LOSS50_ESRD4 ==0, 2*DEAD, 1)) # numeric with 0,1,2 values
event_LOSS50_ESRD4f      <- factor(event_LOSS50_ESRD4x, 0:2, labels=c("censor", "LOSS50_ESRD4", "death"))

df_complete$eTIME_LOSS50_ESRD4 = eTIME_LOSS50_ESRD4
df_complete$event_LOSS50_ESRD4x = event_LOSS50_ESRD4x   # numeric 0:2
df_complete$event_LOSS50_ESRD4f = event_LOSS50_ESRD4f   # factor

#-- 38: Create eTIME_LOSS50_ESRD7, event_LOSS50_ESRD7x and event_LOSS50_ESRD7f
eTIME_LOSS50_ESRD7       <- with(df_complete, ifelse(LOSS50_ESRD7==0, TIME_DEATH, TIME_LOSS50_ESRD7))
event_LOSS50_ESRD7x      <- with(df_complete, ifelse(LOSS50_ESRD7 ==0, 2*DEAD, 1)) # numeric with 0,1,2 values
event_LOSS50_ESRD7f      <- factor(event_LOSS50_ESRD7x, 0:2, labels=c("censor", "LOSS50_ESRD7", "death"))

df_complete$eTIME_LOSS50_ESRD7 = eTIME_LOSS50_ESRD7
df_complete$event_LOSS50_ESRD7x = event_LOSS50_ESRD7x   # numeric 0:2
df_complete$event_LOSS50_ESRD7f = event_LOSS50_ESRD7f   # factor


#-- 39: Create eTIME_LOSS50_ESRD10, event_LOSS50_ESRD10x and event_LOSS50_ESRD10f
eTIME_LOSS50_ESRD10       <- with(df_complete, ifelse(LOSS50_ESRD10==0, TIME_DEATH, TIME_LOSS50_ESRD10))
event_LOSS50_ESRD10x      <- with(df_complete, ifelse(LOSS50_ESRD10 ==0, 2*DEAD, 1)) # numeric with 0,1,2 values
event_LOSS50_ESRD10f      <- factor(event_LOSS50_ESRD10x, 0:2, labels=c("censor", "LOSS50_ESRD10", "death"))

df_complete$eTIME_LOSS50_ESRD10 = eTIME_LOSS50_ESRD10
df_complete$event_LOSS50_ESRD10x = event_LOSS50_ESRD10x   # numeric 0:2
df_complete$event_LOSS50_ESRD10f = event_LOSS50_ESRD10f   # factor


# `df_complete`: Convert to factors. Create weights, if needed etc
set.seed     = 1234
df_complete = within(df_complete,{
   ACRcat   = factor(ACRcat)
   CKD      = factor(CKD)
   #CHF      = factor(CHF)
   AFIB     = factor(AFIB)
   RACE_CAT_1 = factor(RACE_CAT_1)
   wght     = 1.2 # new  variable created
   log_OL1  = log(OL1)
   train_01 = runif(n= nrow(df_complete)) > 0.8 
})

