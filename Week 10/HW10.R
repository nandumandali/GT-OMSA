# -------------------- Code for HW 10 Question 1 -----------------------------
# Clear environment

rm(list = ls())

set.seed(1)

data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data", stringsAsFactors = FALSE,sep=',',header=FALSE)

head(data)

data[which(data$V7 == "?"),]

nrow(data[which(data$V7 == "?"),])/nrow(data)

missing <- which(data$V7 == "?", arr.ind = TRUE)

missing


getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

mode_V7 <- as.numeric(getmode(data[-missing,"V7"]))

mode_V7



data_mode_imp <- data
data_mode_imp[missing,]$V7 <- mode_V7
data_mode_imp$V7 <- as.integer(data_mode_imp$V7)

mean_V7 <- round(as.numeric(mean(as.numeric(data[-missing, "V7"]))))

mean_V7

data_mean_imp <- data
data_mean_imp[missing,]$V7 <- mean_V7
data_mean_imp$V7 <- as.integer(data_mean_imp$V7)


data_regression <- data[-missing, 2:10]
data_regression$V7 <- as.integer(data_regression$V7)

model <- lm(V7~V2+V3+V4+V5+V6+V7+V8+V9+V10, data=data_regression)

summary(model)

step(model)

model2 <- lm(V7~V2+V4+V5+V8, data=data_regression)

summary(model2)

V7_reg <- predict(model2, newdata=data[missing,])

data_reg_imp <- data
data_reg_imp[missing,]$V7 <- V7_reg
data_reg_imp$V7 <- as.numeric(data_reg_imp$V7)

data_reg_imp[missing,]$V7 <- round(V7_reg)
data_reg_imp$V7 <- as.integer(data_reg_imp$V7)

V7_reg_pert <- rnorm(nrow(data[missing,]),V7_reg, sd(V7_reg))
V7_reg_pert

data_pert_imp <- data
data_pert_imp[missing,]$V7 <- V7_reg_pert
data_pert_imp$V7 <- as.numeric(data_pert_imp$V7)

data_pert_imp[missing,]$V7 <- round(V7_reg_pert)
data_pert_imp$V7 <- as.integer(data_pert_imp$V7)

data_pert_imp$V7[data_pert_imp$V7 >10] <- 10
data_pert_imp$V7[data_pert_imp$V7 <1] <- 1

