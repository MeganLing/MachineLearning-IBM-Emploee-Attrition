# **** Ada Boost ****
rm(list=ls())

# ---- Loading ----
load("data_trn.Rdata")
load("data_sud.Rdata")

# ---- Search Space ----
iterList <- c(10, 20, 40, 80, 120, 160, 200)
depthList <- c(10, 15, 20, 25, 30)
splitList <- c(5, 10, 15, 20)

# ---- Grid Search Function ----
source("eval.R")
library(ada)

ada.grid <- function(data, lst.iters, lst.depth, lst.split) {
  # train valid split
  set.seed(5082)
  idx.trn <- sample(nrow(data), 0.7 * nrow(data))
  data.trn <- data[idx.trn, ]
  data.vld <- data[-idx.trn, ]
  
  # init
  param.iters <- c()
  param.depth <- c()
  param.split <- c()
  
  evl.recall <- c()
  evl.precision <- c()
  evl.accuracy <- c()
  evl.weighted <- c()

  # search
  for (iter in lst.iters) { for (depth in lst.depth) { for (split in lst.split) {
    param.iters <- c(param.iters, iter)
    param.depth <- c(param.depth, depth)
    param.split <- c(param.split, split)
    
    model <- ada(Attrition ~ ., data = data.trn, iter = iter, bag.frac = .3,
                 control = rpart.control(maxdepth = depth, cp = .01, xval = 10,
                                         minsplit = split))
    
    p <- predict(model, newdata = data.vld)
    m <- conf.mtx(p, data.vld$Attrition)
    
    evl.recall <- c(evl.recall, m$recall)
    evl.precision <- c(evl.precision, m$precision)
    evl.accuracy <- c(evl.accuracy, m$accuracy)
    evl.weighted <- c(evl.weighted, 1 - weighted.error.rate(m, 0.8))
  }}}
  
  # return dataframe
  return(data.frame(iters = param.iters, depth = param.depth, split = param.split,
                    recall = evl.recall, precision = evl.precision, 
                    accuracy = evl.accuracy, weighted_acc = evl.weighted))
}

# ----Training ----
ada.ori <- ada.grid(data.trn.ori, iterList, depthList, splitList)
ada.over <- ada.grid(data.trn.over, iterList, depthList, splitList)
ada.under <- ada.grid(data.trn.under, iterList, depthList, splitList)
ada.rose <- ada.grid(data.trn.rose, iterList, depthList, splitList)

# ---- Report ----
report.ori <- cbind(data = rep("ori", ncol(ada.ori)), ada.ori)
report.over <- cbind(data = rep("over", ncol(ada.over)), ada.over)
report.under <- cbind(data = rep("under", ncol(ada.under)), ada.under)
report.rose <- cbind(data = rep("rose", ncol(ada.rose)), ada.rose)

report.ada <- rbind(report.ori, report.over, report.under, report.rose)
print.frame(report.ada[order(-report.ada$weighted_acc)[1:10], ])

# ---- Final Model ----
# Original
best <- report.ori[order(-report.ori$weighted_acc)[1], ]
m.ada.ori <- ada(Attrition ~ ., data = data.trn.ori, iter = best$iters, bag.frac = .3,
                  control = rpart.control(maxdepth = best$depth, cp = .01, xval = 10, 
                                          minsplit = best$split))

# Over Sample
best <- report.over[order(-report.over$weighted_acc)[1], ]
m.ada.over <- ada(Attrition ~ ., data = data.trn.over, iter = best$iters, bag.frac = .3,
                 control = rpart.control(maxdepth = best$depth, cp = .01, xval = 10, 
                                         minsplit = best$split))

# Under Sample
best <- report.under[order(-report.under$weighted_acc)[1], ]
m.ada.under <- ada(Attrition ~ ., data = data.trn.under, iter = best$iters, bag.frac = .3,
                  control = rpart.control(maxdepth = best$depth, cp = .01, xval = 10, 
                                          minsplit = best$split))

# ROSE
best <- report.rose[order(-report.rose$weighted_acc)[1], ]
m.ada.rose <- ada(Attrition ~ ., data = data.trn.rose, iter = best$iters, bag.frac = .3,
                   control = rpart.control(maxdepth = best$depth, cp = .01, xval = 10, 
                                           minsplit = best$split))

# ---- Dumping ----
save(m.ada.ori, m.ada.over, m.ada.under, m.ada.rose,
     report.ada, ada.ori, ada.over, ada.under, ada.rose, 
     file = "ada.Rdata")
