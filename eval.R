conf.mtx <- function(pred, true) {
  # get matrix
  mtx <- table(pred, true)
  
  # get values
  tp <- mtx[2, 2]
  tn <- mtx[1, 1]
  fp <- mtx[2, 1]
  fn <- mtx[1, 2]
  
  # compute
  p <- tp + fn
  n <- fp + tn
  
  # accuracy
  accuracy <- (tp + tn) / (p + n) 
  
  # error rate
  error.rate <- (fp + fn) / (p + n) 
  
  # false positive rate
  false.positive.rate <- fp / n
  fallout <- false.positive.rate
  
  # true positive rate
  true.positive.rate <- tp / p
  recall <- true.positive.rate
  sensitivity <- true.positive.rate
  
  # false negative rate
  false.negative.rate <- fn / p
  miss <- false.negative.rate
  
  # true negative rate
  true.negative.rate <- tn / n
  specificity <- true.negative.rate
  
  # positive predictive value
  positive.predictive.value <- tp / (tp + fp)
  precision <- positive.predictive.value
  
  # negative predictive value
  negative.predictive.value <- tn / (tn + fn)
  
  # prediction conditioned fallout
  prediction.conditioned.fallout <- fp / (tp + fp)
  
  # prediction conditioned miss
  prediction.conditioned.miss <- fn / (tn + fn)
  
  # rate of positive prediction
  rate.of.positive.prediction <- (tp + fp) / (p + n)
  detection.prevalence <- rate.of.positive.prediction
  
  # rate of negative prediction
  rate.of.negative.prediction <- (tn + fn) / (p + n)
  
  # prevalence
  prevalence <- (tp + fn) / (p + n)
  
  # detection rate
  detection.rate <- tp / (p + n)
  
  # balanced accuracy
  balanced.accuracy <- (sensitivity + specificity) / 2

  # return list
  return(list(
    matrix = mtx, 
    accuracy = accuracy,
    error.rate = error.rate,
    false.positive.rate = false.positive.rate, 
    fallout = fallout,
    true.positive.rate = true.positive.rate, 
    recall = recall, 
    sensitivity = sensitivity,
    false.negative.rate = false.negative.rate,
    miss = miss,
    true.negative.rate = true.negative.rate,
    specificity = specificity,
    positive.predictive.value = positive.predictive.value,
    precision = precision,
    negative.predictive.value = negative.predictive.value,
    prediction.conditioned.fallout = prediction.conditioned.fallout,
    prediction.conditioned.miss = prediction.conditioned.miss,
    rate.of.positive.prediction = rate.of.positive.prediction,
    detection.prevalence = detection.prevalence,
    rate.of.negative.prediction = rate.of.negative.prediction,
    prevalence = prevalence,
    detection.rate = detection.rate,
    balanced.accuracy = balanced.accuracy
  ))
}

weighted.error.rate <- function(mtx, weight) {
  return(weight * mtx$miss + (1 - weight) * mtx$fallout)
}

eval.frame <- function(true, pred.vect, feats, weight = 0.5) {
  evl.mtx <- matrix(NA, nrow = length(pred.vect), ncol = length(feats) + 1)
  for (i in 1:length(pred.vect)) {
    pred <- pred.vect[[i]]
    mtx <- conf.mtx(pred, true)
    
    for (j in 1:length(feats)) {
      feat <- feats[j]
      evl.mtx[i, j] <- mtx[[feat]]
    }
    
    evl.mtx[i, ncol(evl.mtx)] <- 1 - weighted.error.rate(mtx, weight)
  }
  
  evl.frame <- data.frame(evl.mtx)
  colnames(evl.frame) <- sapply(c(feats, "weighted.accuracy"), gsub, 
                                  pattern = "\\.", replacement = " ")
  rownames(evl.frame) <- names(pred.vect)
  
  return(evl.frame)
}


print.frame <- function(pf) {
  knitr::kable(pf)
}


eval.example <- function () {
  val.true <- c(1, 0, 1, 0, 1, 0)
  val.p1 <- c(1, 0, 1, 0, 1, 0)
  val.p2 <- c(1, 1, 1, 0, 1, 0)
  val.p3 <- c(0, 0, 1, 0, 1, 0)
  val.p4 <- c(0, 1, 0, 1, 0, 1)
  
  ef <- eval.frame(val.true, list(m1=val.p1, m2=val.p2, m3=val.p3, m4=val.p4),
                   c("accuracy", "precision", "recall"), weight = 0.8)
  print.frame(ef)
}
