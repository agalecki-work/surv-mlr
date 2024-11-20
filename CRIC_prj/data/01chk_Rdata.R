sink("01chk_Rdata.R")
getwd()

Rdata1 = "cric_olinknpx_112023_v1.Rdata"  # Original data
cat("===== Rdata name", Rdata1, "\n")
onms = load(Rdata1, verbose= TRUE)
cat("Data name:", onms, "\n")
df1 = get(onms)
cat("Dim: ", dim(df1), "\n")
print(names(df1))


Rdata2 = "02-cric_complete112023.Rdata"  # Complete daat data
cat("===== Rdata name", Rdata2, "\n")
onms = load(Rdata2, verbose= TRUE)
print("object names", onms, "\n")
Info_df_complete
var_labels
dim(df_complete)
names(df_complete)





