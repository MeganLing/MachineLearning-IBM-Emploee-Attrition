# **** Test ****
rm(list=ls())

# ---- Loading ----
source("eval.R")
load("data_tst.Rdata")

# ---- Plot ROC ----
library(verification)
plot.roc <- function (m) {
  p <- predict(m, data.tst.ori, type = "prob")
  roc.plot(as.integer(as.factor(data.tst.ori$Attrition)) - 1, p[, 2], main = "")
}

rf <- randomForest(Attrition ~ .,data = data.trn.ori,
                  replace = TRUE, importance = TRUE,
                  ntree = 500, mtry = 6)
plot.roc(rf)

# ---- Random Forest ----
load("rf.Rdata")

p.rf.ori <- predict(m.rf.ori, newdata = data.tst.ori)
p.rf.over <- predict(m.rf.over, newdata = data.tst.ori)
p.rf.under <- predict(m.rf.under, newdata = data.tst.ori)
p.rf.rose <- predict(m.rf.rose, newdata = data.tst.ori)

p <- list(rf.ori = p.rf.ori, 
          rf.over = p.rf.over,
          rf.under = p.rf.under, 
          rf.rose = p.rf.rose)
e <- eval.frame(data.tst.ori$Attrition, p,
                c( "recall", "precision", "accuracy"), weight = 0.8)
print.frame(e)

# ---- Ada Boost ----
load("ada.Rdata")

p.ada.ori <- predict(m.ada.ori, newdata = data.tst.ori)
p.ada.over <- predict(m.ada.over, newdata = data.tst.ori)
p.ada.under <- predict(m.ada.under, newdata = data.tst.ori)
p.ada.rose <- predict(m.ada.rose, newdata = data.tst.ori)

p <- list(ada.ori = p.ada.ori, 
          ada.over = p.ada.over, 
          ada.under = p.ada.under, 
          ada.rose = p.ada.rose)
e <- eval.frame(data.tst.ori$Attrition, p,
                c( "recall", "precision", "accuracy"), weight = 0.8)
print.frame(e)


# ---- SVM ---
load("svm.Rdata")

p.svm.l <- predict(best_l, newdata = data.tst.dmy)
p.svm.r <- predict(best_r, newdata = data.tst.dmy)

p.svm.l.50 <- predict(best50_l, newdata = data.tst.pca.50)
p.svm.r.50 <- predict(best50_r, newdata = data.tst.pca.50)

p.svm.l.80 <- predict(best80_l, newdata = data.tst.pca.80)
p.svm.r.80 <- predict(best80_r, newdata = data.tst.pca.80)

p.svm.l.95 <- predict(best95_l, newdata = data.tst.pca.95)
p.svm.r.95 <- predict(best95_r, newdata = data.tst.pca.95)

p <- list(svm.l = p.svm.l,
          svm.r = p.svm.r,
          svm.l.50 = p.svm.l.50,
          svm.r.50 = p.svm.r.50,
          svm.l.80 = p.svm.l.80,
          svm.r.80 = p.svm.r.80,
          svm.l.95 = p.svm.l.95,
          svm.r.95 = p.svm.r.95)
e <- eval.frame(data.tst.ori$Attrition, p,
                c( "recall", "precision", "accuracy"), weight = 0.8)
print.frame(e)
