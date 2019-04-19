# **** Training Testing Split ****
rm(list=ls())

# ---- Loading ----
load("data_ori.Rdata")
load("data_pca.Rdata")

# ---- Seed ----
set.seed(5082)

# ---- Splitting ----
idx.trn <- sample(nrow(data.ori), 0.7 * nrow(data.ori))
idx.tst <- setdiff(1:nrow(data.ori), idx.trn)

data.trn.ori <- data.ori[idx.trn, ]
data.tst.ori <- data.ori[idx.tst, ]

data.trn.dmy <- data.dmy[idx.trn, ]
data.tst.dmy <- data.dmy[idx.tst, ]

data.trn.pca.50 <- data.pca.50[idx.trn, ]
data.tst.pca.50 <- data.pca.50[idx.tst, ]

data.trn.pca.80 <- data.pca.80[idx.trn, ]
data.tst.pca.80 <- data.pca.80[idx.tst, ]

data.trn.pca.95 <- data.pca.95[idx.trn, ]
data.tst.pca.95 <- data.pca.95[idx.tst, ]

# ---- Dumping ---
save(data.trn.ori, data.trn.dmy, data.trn.pca.50, data.trn.pca.80, data.trn.pca.95,
     file = "data_trn.Rdata")
save(data.tst.ori, data.tst.dmy, data.tst.pca.50, data.tst.pca.80, data.tst.pca.95,
     file = "data_tst.Rdata")
