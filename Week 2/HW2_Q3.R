
#clear environment
rm(list=ls())


library(datasets)

# Removing the response values from the dataset
my_iris <- iris[-5]


# Setting the random number generator seed so that our results are reproducible

set.seed(100)


# Trying to find the number of clusters to be used
nc <- 15
dist <- (nrow(my_iris)-1)*sum(apply(my_iris,2,var))
  for (i in 2:nc){
    dist[i] <- sum(kmeans(my_iris, centers=i)$withinss)}
  plot(1:nc, dist, type="b", xlab="Number of Clusters",
       ylab="Total Distance")
  
# From the graph, it can be inferred that 7 clusters will be a better value for k. K-means clustering of the entire
# iris data set with k=7
  
irisCluster1 <- kmeans(my_iris, 7, nstart=20)

table(irisCluster1$cluster,iris$Species)

# K-means clustering of iris data set with k=7 and petal length, petal width as predictors

irisCluster2 <- kmeans(my_iris[,3:4], 7, nstart=20)

table(irisCluster2$cluster,iris$Species)

# K-means clustering of iris data set with k=7 and sepal length, sepal width as predictors

irisCluster3 <- kmeans(my_iris[,1:2], 7, nstart=20)

table(irisCluster3$cluster,iris$Species)

# K-means clustering of iris data set with k=3 and petal length, petal width as predictors

irisCluster4 <- kmeans(my_iris[,3:4], 3, nstart=20)

table(irisCluster4$cluster,iris$Species)
