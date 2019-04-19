# **** Sampling Unbalanced Dataset ****
rm(list=ls())

# ---- Loading ----
load("data_trn.Rdata")
cnt <- table(data.trn.ori$Attrition)

# ---- Library ----
library(ROSE)

# ---- Over Sampling ----
data.trn.over <- ovun.sample(Attrition ~ ., data = data.trn.ori,
                             method = 'over', N = 2 * max(cnt))$data

# ---- Under Sampling ----
data.trn.under <- ovun.sample(Attrition ~ ., data = data.trn.ori,
                              method = 'under', N = 2 * min(cnt))$data

# ---- ROSE .5 ----
data.trn.rose <- ovun.sample(Attrition ~ ., data = data.trn.ori,
                             method = 'both', p = .5)$data

# ---- Dumping ----
save(data.trn.over, data.trn.under, data.trn.rose, file = "data_sud.Rdata")
