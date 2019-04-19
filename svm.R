# ---- Load Data ----
load("data_trn.Rdata")
load("data_tst.Rdata")
data.trn.dmy$Attrition <- as.factor(data.trn.dmy$Attrition)
data.tst.dmy$Attrition <- as.factor(data.tst.dmy$Attrition)
# ---- without pca ----
tune_l <- tune(svm,Attrition ~ ., data=data.trn.dmy, kernel="linear", 
               ranges=list(cost=c(0.001, 0.01, 0.1, 1,5, 10, 100)))
summary(tune_l)
best_l <- tune_l$best.model
tune_r <- tune(svm, Attrition~., data=data.trn.dmy, kernel="radial", 
               ranges=list(cost=c(0.1, 1, 10, 100, 1000),
                           gamma=c(0.5, 1, 2, 3, 4)))
summary(tune_r)
best_r <- tune_r$best.model

## ---- evalution ----
test_l <- predict(best_l,data.tst.dmy)
(table(data.tst.dmy$Attrition,test_l))
test_r <- predict(best_r,data.tst.dmy)
(table(data.tst.dmy$Attrition,test_r))

# ---- pca 50 ----
tune50_l <- tune(svm,Attrition ~ ., data=data.trn.pca.50, kernel="linear", 
                 ranges=list(cost=c(0.001, 0.01, 0.1, 1,5, 10, 100)))
summary(tune50_l)
best50_l <- tune50_l$best.model
tune50_r <- tune(svm, Attrition~., data=data.trn.pca.50, kernel="radial", 
                 ranges=list(cost=c(0.1, 1, 10, 100, 1000),
                             gamma=c(0.5, 1, 2, 3, 4)))
summary(tune50_r)
best50_r <- tune50_r$best.model

## ---- evalution ----
test50_l <- predict(best50_l,data.tst.pca.50)
(table(data.tst.pca.50$Attrition,test50_l))
test50_r <- predict(best50_r,data.tst.pca.50)
(table(data.tst.pca.50$Attrition,test50_r))

# ---- pca 80 ----
tune80_l <- tune(svm,Attrition ~ ., data=data.trn.pca.80, kernel="linear", 
                 ranges=list(cost=c(0.001, 0.01, 0.1, 1,5, 10, 100)))
summary(tune80_l)
best80_l <- tune80_l$best.model
tune80_r <- tune(svm, Attrition~., data=data.trn.pca.80, kernel="radial", 
                 ranges=list(cost=c(0.1, 1, 10, 100, 1000),
                             gamma=c(0.5, 1, 2, 3, 4)))
summary(tune80_r)
best80_r <- tune80_r$best.model

## ---- evalution ----
test80_l <- predict(best80_l,data.tst.pca.80)
(table(data.tst.pca.80$Attrition,test80_l))
test80_r <- predict(best80_r,data.tst.pca.80)
(table(data.tst.pca.80$Attrition,test80_r))

# ---- pca 95 ----
tune95_l <- tune(svm,Attrition ~ ., data=data.trn.pca.95, kernel="linear", 
                 ranges=list(cost=c(0.001, 0.01, 0.1, 1,5, 10, 100)))
summary(tune95_l)
best95_l <- tune95_l$best.model
tune95_r <- tune(svm, Attrition~., data=data.trn.pca.95, kernel="radial", 
                 ranges=list(cost=c(0.1, 1, 10, 100, 1000),
                             gamma=c(0.5, 1, 2, 3, 4)))
summary(tune95_r)
best95_r <- tune95_r$best.model

## ---- evalution ----
test95_l <- predict(best95_l,data.tst.pca.95)
(table(data.tst.pca.95$Attrition,test95_l))
test95_r <- predict(best95_r,data.tst.pca.95)
(table(data.tst.pca.95$Attrition,test95_r))
