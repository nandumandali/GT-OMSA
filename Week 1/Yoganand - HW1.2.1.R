library(kernlab)

#Reading the input data

mydata <- read.table("https://d37djvu3ytnwxt.cloudfront.net/assets/courseware/v1/e39a3df780dacd5503df6a8322d72cd2/asset-v1:GTx+ISYE6501x+3T2017+type@asset+block/credit_card_data-headers.txt", header = TRUE)

mydata <- as.matrix(mydata)

# call ksvm. Vanilladot is a simple linear kernel.
model <- ksvm(mydata[, 1:10], mydata[,11], type="C-svc", kernel= "vanilladot", C=100, scaled=TRUE)

# calculate a1.am
a <- colSums(mydata[model@SVindex,1:10] * model@coef[[1]])
print("a =")
print(a)

# calculate a0
a0 <- -model@b
print("a0 =")
print(a0)

# see what the model predicts
pred <- predict(model, mydata[,1:10])
print("pred =")
print(pred)

# see what fraction of the model's predictions match the
# actual classification
print("Prediction % =")
print(sum(pred == mydata[,11]) / nrow(mydata))



