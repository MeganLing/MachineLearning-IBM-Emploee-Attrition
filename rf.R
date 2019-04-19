# **** Random Forest ****
rm(list=ls())

# ---- Loading ----
load("data_trn.Rdata")
load("data_sud.Rdata")

# ---- Hyper Parameter ----
ntree <- 1000
n.feats <- ncol(data.trn.ori) - 1
mtry <- round(c(1, n.feats, sqrt(n.feats), n.feats / 2))
color <- c("red", "blue", "green", "darkviolet")

# ---- OOB Function ----
library(randomForest)

plot.oob <- function(data, ntree, color, weight, ymax) {
  set.seed(5082)
  plot(1, type = "n", xlab = "number of trees", ylab = "weighted error rate", 
       xlim = c(0, ntree + 1), ylim = c(0, ymax))
  
  score = c()
  mtrys = c()
  for (i in 1:length(mtry)) {
    rf <- randomForest(Attrition ~ .,data = data,
                       ntree = ntree, mtry = mtry[i], importance = TRUE)
    
    scores <- weight * rf$err.rate[, 2] +  (1 - weight) * rf$err.rate[, 3]
    score <- c(score, min(scores))
    mtrys <- c(mtrys, which.min(scores))
    lines(scores, col = color[i])
  }
  
  # legend("topright", col = color, lty=1:2, cex=0.8, legend = c("", "", "", ""))
  return(list(score = score, mtrys = mtrys))
}

# ---- Original ----
r <- plot.oob(data.trn.ori, ntree, color, weight = .2, ymax = 1)
m.rf.ori <- randomForest(Attrition ~ .,data = data.trn.ori, 
                         replace = TRUE, importance = TRUE,
                         ntree = r$mtrys[which.min(r$score)],
                         mtry = mtry[which.min(r$score)])


# ---- Over Sampling ----
r <- plot.oob(data.trn.over, ntree, color, weight = .2, ymax = .06)
m.rf.over <- randomForest(Attrition ~ .,data = data.trn.over, 
                          replace = TRUE, importance = TRUE,
                          ntree = r$mtrys[which.min(r$score)], 
                          mtry = mtry[which.min(r$score)])

# ---- Under Sampling ----
r <- plot.oob(data.trn.under, ntree, color, weight = .2, ymax = .5)
m.rf.under <- randomForest(Attrition ~ .,data = data.trn.under, 
                           replace = TRUE, importance = TRUE,
                           ntree = r$mtrys[which.min(r$score)],
                           mtry = mtry[which.min(r$score)])

# ---- ROSE ----
r <- plot.oob(data.trn.rose, ntree, color, weight = .2, ymax = .25)
m.rf.rose <- randomForest(Attrition ~ .,data = data.trn.rose, 
                          replace = TRUE, importance = TRUE,
                          ntree = r$mtrys[which.min(r$score)],
                          mtry = mtry[which.min(r$score)])

# ---- Dumping ----
save(m.rf.ori, m.rf.over, m.rf.under, m.rf.rose, file = "rf.Rdata")
