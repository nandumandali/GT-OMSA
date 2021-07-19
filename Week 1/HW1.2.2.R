
library(kknn)

#Reading the input data
mydata<-read.table("https://d37djvu3ytnwxt.cloudfront.net/assets/courseware/v1/e39a3df780dacd5503df6a8322d72cd2/asset-v1:GTx+ISYE6501x+3T2017+type@asset+block/credit_card_data-headers.txt", header =T, sep='\t')

# Number of rows in mydata
m <- nrow(mydata)

#Create an empty vector
predicted <- rep(0,(m))

#Run the for loop, excluding the ith row in ith iteration. 
for (i in 1:m){
  # Call kknn function 
  model <- kknn(mydata[-i,11]~., mydata[-i,1:10], mydata[i,1:10], k=5, kernel="optimal",scale=TRUE)
  
  #Use round function to convert continuous values to discrete 1's or 0's
  predicted[i] <- round(fitted(model))
}

# Calcuating the fraction of predictions that match the actual response 
print(sum(predicted == mydata[,11]) / nrow(mydata))


