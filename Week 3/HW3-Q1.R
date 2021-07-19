# -------------------- Code for Question 1 -----------------------------
# Clear environment

rm(list = ls())

library(outliers)


my_data <- read.table("http://www.statsci.org/data/general/uscrime.txt", header = TRUE)

head(my_data)

shapiro.test(my_data$Crime)

qqnorm(my_data$Crime)

# NULL hypothesis - no outlier in both tails
grubbs.test(my_data$Crime, type=11,opposite=FALSE,two.sided=FALSE)


# NULL hypothesis - no outlier in one tail
grubbs.test(my_data$Crime, type=10,opposite=FALSE,two.sided=FALSE)

# NULL hypothesis - no outlier in one tail
grubbs.test(my_data$Crime[-4], type=10,opposite=FALSE,two.sided=FALSE)

# NULL hypothesis - no outlier in one tail
grubbs.test(my_data$Crime, type=10,opposite=TRUE,two.sided=FALSE)

