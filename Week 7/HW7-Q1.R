# -------------------- Code for HW 7 Question 2 -----------------------------
# Clear environment

rm(list = ls())

set.seed(1)

library (tree)

crime.data <- read.table("http://www.statsci.org/data/general/uscrime.txt", header=TRUE)

tree.data <- tree(Crime~.,data=crime.data)

summary(tree.data)

plot(tree.data)
text(tree.data)

tree.data$frame

yhat <- predict(tree.data)

plot(crime.data$Crime, yhat)
abline(0,1)

SSres <- sum((yhat - crime.data$Crime)^2)
SStot <- sum((crime.data$Crime -mean(crime.data$Crime))^2)
r2 <- 1 - SSres/SStot
r2

prune.data <- prune.tree(tree.data, best=2)
plot(prune.data)
text(prune.data)


prune.tree(tree.data)$size
prune.tree(tree.data)$dev


cv.data <- cv.tree(tree.data)
cv.data$size
cv.data$dev

prune.data
x1 <- crime.data[which(prune.data$where == 2),]
m1 <- lm(Crime~.,data=x1)
summary (m1)


m1v2 <- lm(Crime~Ed+Pop+Prob+Time,data=x1)
summary(m1v2)

m1v3 <- lm(Crime~Pop,data=x1)
summary(m1v3)

c1d<-cv.lm(x1, m1v3, m=nrow(x1))
1 -attr(c1d, "ms")*nrow(x1)/sum((x1$Crime - mean(x1$Crime))^2)

rm(list = ls())

crime.data <- read.table("http://www.statsci.org/data/general/uscrime.txt", header=TRUE)


library(randomForest)

set.seed(1)

numpred <- 4

rf.data <- randomForest(Crime~.,data=crime.data, ntry=numpred, importance =TRUE,
                        ntree=500)

rf.data

yhat.rf <- predict(rf.data)

SSres <- sum((yhat.rf-crime.data$Crime)^2)
SStot <- sum((crime.data$Crime-mean(crime.data$Crime))^2)

r2 <- 1 - SSres/SStot

r2

SSres <- 0

for (i in 1:nrow(crime.data)){
  rf.x <- randomForest(Crime~.,data=crime.data[-i,],ntry=numpred, importance=TRUE)
  SSres = SSres + (predict(rf.x, newdata=crime.data[i,])-crime.data[i,16])^2
}
r2 <- 1 - SSres/SStot

r2
importance(rf.data)

varImpPlot(rf.data)

