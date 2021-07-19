# -------------------- Code for HW 6 Question 2 -----------------------------
# Clear environment

rm(list = ls())

set.seed(1)

crimedata <- read.table("http://www.statsci.org/data/general/uscrime.txt", header=TRUE)

for (i in 1:15){
  for (j in 1:15){
    if(i!=j){
      plot(crimedata[,i],crimedata[,j],main="Scatterplot Example", xlab=colnames(crimedata)[i],ylab=colnames(crimedata)[j], pch=19)
      
    }
  }
}

pca <- prcomp(crimedata[, 1:15],scale.=TRUE)

summary(pca)

screeplot(pca, type="lines",col="blue")

var <- pca$sdev^2

propvar <- var/sum(var)

plot(propvar, xlab= "Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type="b")

cumsum(propvar)

plot(cumsum, xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim= c(0,1),type="b")

PCs <- pca$x[,1:4]

PCs[1,]

PCcrime <- cbind(PCs, crimedata[,16])

PCcrime

as.data.frame(PCcrime)

model <- lm(V5~.,data=as.data.frame(PCcrime))
summary(model)

beta0 <- model$coefficients[1]
betas <- model$coefficients[2:5]
beta0
betas

pca$rotation[,1:4]
alphas <- pca$rotation[,1:4] %*% betas
t(alphas)






