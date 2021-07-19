rm(list=ls())

data <- read.table("https://d37djvu3ytnwxt.cloudfront.net/assets/courseware/v1/a145a478beb6f64b59ec1de082b84235/asset-v1:GTx+ISYE6501x+3T2017+type@asset+block/germancredit.txt",sep=" ")
head(data)

data$V21[data$V21==1]<-0
data$V21[data$V21==2] <- 1

set.seed(1)

trn <- sample(1:nrow(data),size=round(0.7*(nrow(data))))

d.learn<-data[trn,]
d.valid<-data[-trn,]

model = glm(V21 ~.,family=binomial(link="logit"),data=d.learn)
summary(model)

step(model)

model = glm(formula = V21 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V10 + 
              V12 + V14 + V15 + V16 + V19 + V20, family = binomial(link = "logit"), 
            data = d.learn)

summary(model)

d.learn$V1A13[d.learn$V1 =="A13"]<-1
d.learn$V1A13[d.learn$V1 !="A13"]<-0

d.learn$V1A14[d.learn$V1 =="A14"]<-1
d.learn$V1A14[d.learn$V1 !="A14"]<-0

head(d.learn)

y_hat <- predict(model,d.valid,type="response")


y_pred<-as.integer(y_hat >0.5)
y_pred


table(y_pred,d.valid$V21)

library(pROC)
AUC <- roc(d.valid$V21, y_pred)
plot(AUC,main="ROC Curve")

y_pred2 <- as.integer(y_hat> 0.4)
table <- as.matrix(table(y_pred2,d.valid$V21))
cost <- table[2,1]+5*table[1,2]
cost

