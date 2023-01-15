XDATA <- read.csv(file = "C:/THT/subsonic_airfoil_data_set.csv")
NIM <- 77
set.seed(NIM)
idx <- sample(8000)
nsamp <- 1000
Xsub <- XDATA[idx[1:nsamp],]

head(Xsub)

#par(mfrow=c(1,9))
plot(Xsub$rle, Xsub$LperD, pch=21)
cor(Xsub$rle, Xsub$LperD, method="pearson")

plot(Xsub$xup, Xsub$LperD, pch=21, col="navy")
cor(Xsub$xup, Xsub$LperD, method="pearson")

plot(Xsub$zup, Xsub$LperD, pch=21, col="green")
cor(Xsub$zup, Xsub$LperD, method="pearson")

plot(Xsub$zxxup, Xsub$LperD, pch=21, col="orange")
cor(Xsub$zxxup, Xsub$LperD, method="pearson")

plot(Xsub$xlo, Xsub$LperD, pch=21, col="purple")
cor(Xsub$xlo, Xsub$LperD, method="pearson")

plot(Xsub$zlo, Xsub$LperD, pch=21, col="cyan")
cor(Xsub$zlo, Xsub$LperD, method="pearson")

plot(Xsub$zxxlo, Xsub$LperD, pch=21, col="red")
cor(Xsub$zxxlo, Xsub$LperD, method="pearson")

plot(Xsub$alphate, Xsub$LperD, pch=21, col="salmon")
cor(Xsub$alphate, Xsub$LperD, method="pearson")

plot(Xsub$betate, Xsub$LperD, pch=21, col="brown")
cor(Xsub$betate, Xsub$LperD, method="pearson")

plot(Xsub$aoa, Xsub$LperD, pch=21, col="grey")
cor(Xsub$aoa, Xsub$LperD, method="pearson")

MLRmod = lm(LperD~rle+xup+zup+zxxup+xlo+zlo+zxxlo+alphate+betate+aoa, Xsub)
print(MLRmod)
summary(MLRmod)

min_max_norm <- function(Xsub){
  for (i in 2:(dim(Xsub)[2]-1)){
    X <- Xsub[,i]
    Xsub[,i] <- (X-min(X))/(max(X)-min(X))
  }
  return(Xsub)
}

Xsubnorm <- min_max_norm(Xsub)

MLRmod2 = lm(LperD~rle+xup+zup+zxxup+xlo+zlo+zxxlo+alphate+betate+aoa, Xsubnorm)
print(MLRmod2)
summary(MLRmod2)