# -------------------- Code for HW 5 Question 2 -----------------------------
# Clear environment

rm(list = ls())

set.seed(1)

crimedata <- read.table("http://www.statsci.org/data/general/uscrime.txt", header=TRUE)

head(crimedata)

# Create a linear regression model 
model <- lm(Crime~., data=crimedata)

#Summary of the model
summary(model)


# Test data point to check the fit of the model
test <- data.frame(M=14.0, So=0, Ed=10.0, Po1=12.0,Po2=15.5,LF=0.64,M.F=94.0,Pop=150,NW=1.1,U1=0.120,U2=3.6,Wealth=3200,Ineq=20.1,Prob=0.04,Time=39.0)

# Predict the crime value for the test data point
pred_model <- predict(model, test)

pred_model

library(DAAG)

# Cross-validation on the model

cfirst <- cv.lm(crimedata,model,m=5)

# mean squared error times the number of data points, gives sum of squared errors

SSres_cfirst <- attr(cfirst,"ms")*nrow(crimedata)

# total sum of squared differences between data and its mean

SStot <- sum((crimedata$Crime - mean(crimedata$Crime))^2)


# Cross-validated R^2
1 - SSres_cfirst/SStot

# Linear regression removing insignificant factors based on p-values. Multiple iterations until the model contains only significant factors

model2<- lm(Crime~M+Ed+Po1+Po2+M.F+Pop+NW+U1+U2+Wealth+Ineq+Prob, data=crimedata)

model3<- lm(Crime~M+Ed+Po1+Po2+M.F+Pop+U1+U2+Wealth+Ineq+Prob, data=crimedata)

model4<- lm(Crime~M+Ed+Po1+M.F+Pop+U1+U2+Wealth+Ineq+Prob, data=crimedata)

model5<- lm(Crime~M+Ed+Po1+U1+U2+Wealth+Ineq+Prob, data=crimedata)

model6<- lm(Crime~M+Ed+Po1+U2+Ineq+Prob, data=crimedata)

#Summary of the model
summary(model6)

# Predict the crime value for the test data point
pred_model <- predict(model6, test)

pred_model


# Cross-validation on the model
cfirst <- cv.lm(crimedata,model6,m=5)

# mean squared error times the number of data points, gives sum of squared errors
SSres_cfirst <- attr(cfirst,"ms")*nrow(crimedata)

# total sum of squared differences between data and its mean
SStot <- sum((crimedata$Crime - mean(crimedata$Crime))^2)

# Cross-validated R^2
1 - SSres_cfirst/SStot


