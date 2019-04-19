library(verification)
m.rf.ori
test.pred <- predict(m.rf.ori,data.tst.ori,type = "prob")
roc.plot <- roc.plot(as.integer(as.factor(data.tst.ori$Attrition))-1,test.pred,main = "")
