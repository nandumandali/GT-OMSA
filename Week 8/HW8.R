# -------------------- Code for HW 8 Question 1 -----------------------------
# Clear environment

rm(list = ls())

set.seed(1)

data <- read.table("http://www.statsci.org/data/general/uscrime.txt", header=TRUE)

scaledData = as.data.frame(scale(data[,c(1,3:15)]))
scaledData <- cbind(data[,2], scaledData, data[,16])
colnames(scaledData)[1] <- "So"
colnames(scaledData)[16] <- "Crime"

library(caret)

model <- lm(Crime ~., data=data)
model <- lm(Crime ~., data=scaledData)
step(model, direction = "backward")

model_step1 <- lm(Crime~ M.F+U1+Prob+U2+M+Ed+Ineq+Po1, data=data)
model_step2 <- lm(Crime~ M.F+U1+Prob+U2+M+Ed+Ineq+Po1, data=scaledData)

summary(model_step1)
summary(model_step2)

SST <- sum((data$Crime - mean(data$Crime))^2)
SSE <- 0

for (i in 1:nrow(scaledData)){
  model_step_i <- lm(Crime~ M.F+U1+Prob+U2+M+Ed+Ineq+Po1, data=scaledData[-i,])
  pred_i <- predict(model_step_i, newdata=scaledData[i,])
  SSE <- SSE + ((pred_i - data[i,16])^2)
}


R2_step <- 1 - SSE/SST
R2_step

model_step3<- lm(Crime~M+Ed+Po1+U2+Ineq+Prob, data=scaledData)

summary(model_step3)

SST <- sum((data$Crime - mean(data$Crime))^2)
SSE <- 0

for (i in 1:nrow(scaledData)){
  model_step_i <- lm(Crime~M+Ed+Po1+U2+Ineq+Prob, data=scaledData[-i,])
  pred_i <- predict(model_step_i, newdata=scaledData[i,])
  SSE <- SSE + ((pred_i - data[i,16])^2)
}


R2_step <- 1 - SSE/SST
R2_step

library(glmnet)

set.seed(1)

lasso <- cv.glmnet(x=as.matrix(scaledData[,-16]), y=as.matrix(scaledData$Crime), 
                   alpha=1, nfolds=5, type.measure="mse",family="gaussian")

lasso
lasso$lambda.min
lasso$cvm


lasso_no_cv <- glmnet(x=as.matrix(scaledData[,-16]), y=as.matrix(scaledData$Crime), 
                      alpha=1,family="gaussian")

lasso_no_cv

coef_lasso <- coef(lasso, s=lasso$lambda.min)

coef_lasso

model_lasso <- lm(Crime ~ So+M+Ed+Po1+LF+M.F+Pop+NW+U1+U2+Wealth+Ineq+Prob, data=scaledData)

summary(model_lasso)

SST <- sum((data$Crime- mean(data$Crime))^2)

SSE <- 0 

for (i in 1:nrow(scaledData)){
  model_lasso_i <- lm(Crime ~ So+M+Ed+Po1+LF+M.F+Pop+NW+U1+U2+Wealth+Ineq+Prob, data=scaledData[-i,])
  pred_i <- predict(model_lasso_i, newdata=scaledData[i,])
  SSE <- SSE + ((pred_i - data[i,16])^2)
}

R2_lasso <- 1 - SSE/SST
R2_lasso


library(glmnet)

set.seed(1)

alpha <- 0.5

elastic_net <- cv.glmnet(x=as.matrix(scaledData[,-16]), y=as.matrix(scaledData$Crime), 
                                alpha=alpha, nfolds=5, type.measure="mse",family="gaussian")

coef_elasticnet <- coef(elastic_net, s=elastic_net$lambda.min)

coef_elasticnet

model_elastic_net <- lm(Crime ~ So+M+Ed+Po1+Po2+M.F+Pop+NW+U1+U2+Wealth+Ineq+Prob, data=scaledData)

summary(model_elastic_net)

SST <- sum((data$Crime- mean(data$Crime))^2)

SSE <- 0 

for (i in 1:nrow(scaledData)){
  model_elastic_net_i <- lm(Crime ~ So+M+Ed+Po1+Po2+LF+M.F+Pop+NW+U1+U2+Wealth+Ineq+Prob, data=scaledData[-i,])
  pred_i <- predict(model_elastic_net_i, newdata=scaledData[i,])
  SSE <- SSE + ((pred_i - data[i,16])^2)
}

R2_elastic_net <- 1 - SSE/SST
R2_elastic_net


