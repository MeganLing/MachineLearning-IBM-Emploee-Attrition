# **** Principal Component Analysis ****
rm(list=ls())

# ---- Loading ----
load("data_ori.Rdata")

# ---- Factors expanding ----
mtx <- model.matrix(Attrition ~ ., data = data.ori)[, -1]

data.dmy <- data.frame(cbind(data.ori$Attrition, mtx))
colnames(data.dmy)[1] <-"Attrition"

# ---- Principal Component Analysis ----
pr.out <- prcomp(mtx, scale = TRUE)

pr.var <- pr.out$sdev^2
pve <- pr.var/sum(pr.var)

plot(cumsum(pve), ylim = c(0,1), type = 'l', col = "blue",
     xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained")

# ---- Selection Function ----
pca.dataframe <- function(y, col.name, pr.out, rate) {
  pr.var <- pr.out$sdev^2
  pve <- pr.var/sum(pr.var)
  
  pca.k <- min(which(cumsum(pve) > rate))
  data.pca <- data.frame(pr.out$x[, 1:pca.k])
  
  data.pca <- cbind(y, data.pca)
  colnames(data.pca)[1] <- col.name
  
  return(data.pca)
}

# ---- PCA Explained ----
data.pca.50 <- pca.dataframe(data.ori$Attrition, "Attrition", pr.out, 0.50)
data.pca.80 <- pca.dataframe(data.ori$Attrition, "Attrition", pr.out, 0.80)
data.pca.95 <- pca.dataframe(data.ori$Attrition, "Attrition", pr.out, 0.95)

# ---- Dumping ---
save(data.dmy, data.pca.50, data.pca.80, data.pca.95, file = "data_pca.Rdata")
