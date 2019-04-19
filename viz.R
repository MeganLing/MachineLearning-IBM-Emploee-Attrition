# **** Data Visualization ****
rm(list=ls())

# ---- Loading ----
load("data_ori.Rdata")

# ---- Feature Types ----
# get numeric features
data.nums <- unlist(lapply(data.ord, is.numeric))

# get ordered features
data.ords <- unlist(lapply(data.ord, is.ordered))

# ---- Correlation ----
# plot correlation matrix
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(data.ord[, data.nums], type = "full")
