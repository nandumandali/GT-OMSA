D
#clear environment
rm(list=ls())

# Load the kernlab library (which contains the kvsm function) and kknn library (which contains kknn function) 
# and read input data 

library(kernlab)
library(kknn)

# ---------------------------- Data manipulation -------------------------------------
mydata <- read.table("https://d37djvu3ytnwxt.cloudfront.net/assets/courseware/v1/e39a3df780dacd5503df6a8322d72cd2/asset-v1:GTx+ISYE6501x+3T2017+type@asset+block/credit_card_data-headers.txt", header = TRUE)

#
# optional check to make sure the data is read correctly
#


head(mydata)

# Setting the random number generator seed so that our results are reproducible

set.seed(1)


# Discrete values of k-fold and k value for kknn model are defined in vectors
kfolds <- c(2,3,4,5,8,10,15,20)
kvalues <- c(2,3,4,5,8,10,12,15,20)

# Empty matrix to capture accuracy of each iteration
acc <- matrix(nrow=8,ncol=9)

# -------------------------- Creating the models ------------------------------------

# Running the nested for loop to find model accuracy with changing k-fold and k values

for (i in 1:length(kfolds)) {
  for (j in 1:length(kvalues))
  {
    model_kknn <- cv.kknn(R1~.,mydata,kcv=kfolds[i],k=kvalues[j],scale=TRUE)
    
    # Converting the output model into dataframe
    results <- as.data.frame(model_kknn)
    
    # round off to 0 or 1
    fit <- round(results$yhat)
    
    # Calculate the fraction of correct predictions
    acc[i,j]<- sum(mydata$R1==fit)/nrow(mydata)
  }
}

# Accuracy matrix
acc 

# Check the range of accuracy to find the least and highest
range(acc)


# Create empty list to record accuracies of ksvm and kknn models
acc_kknn <- list()
acc_ksvm <- list()

# Randomly collecting row indices of 80% data to split between traininf test
rowindices <- sample(1:nrow(mydata), round(.8*nrow(mydata)), replace=FALSE)

# 80% for Training and Validation 
tv_data <- mydata[rowindices,]

# 20% data for Testing
test_data <- mydata[-rowindices,]

#Randomly shuffle the data
tv_data<-tv_data[sample(nrow(tv_data)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(tv_data)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation on both ksvm and kknn models
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  validData <- tv_data[testIndexes, ]
  trainData <- tv_data[-testIndexes, ]
  
  # Cross validation of kknn model. k value of 15 is chosen based on HW1
  model_cv.kknn <- kknn(R1~.,trainData,validData,k=15,scale=TRUE)
  predicted_kknn<- as.integer(fitted(model_cv.kknn)+0.5)
  acc_kknn[i]<- (sum(predicted_kknn == validData[,11])/nrow(validData))
  
  # Cross validation of ksvm model. C value of 100 is chosen based on HW1
  model_cv.ksvm <- ksvm(as.matrix(trainData[,1:10]), trainData[,11], type="C-svc", kernel= "vanilladot", C=100, scaled=TRUE)
  predicted_ksvm <- predict(model_cv.ksvm,validData[,1:10])
  acc_ksvm[i]<- (sum(predicted_ksvm == validData[,11])/nrow(validData))
}


acc_kknn <- unlist(acc_kknn)
acc_ksvm <- unlist(acc_ksvm)

# Mean value will be the accuracy of the kknn model developed using training data 
# and validated against validation data
mean(acc_kknn) 

# Mean value will be the accuracy of the ksvm model developed using training data 
# and validated against validation data
mean(acc_ksvm)

# Final model chosen based on mean accuracies calculated earlier and trained on 80% data
model_final <- ksvm(as.matrix(tv_data[,1:10]), tv_data[,11], type="C-svc", kernel= "vanilladot", C=100, scaled=TRUE)
predicted_model <- predict(model_cv.ksvm,test_data[,1:10])
model_accuracy<- (sum(predicted_model == test_data[,11])/nrow(test_data))

# This accuracy is how good the classifer is.
model_accuracy

