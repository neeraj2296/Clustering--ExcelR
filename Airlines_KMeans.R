library(plyr)
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Importing the data set <<<<<<<<<<<<<<<<<<<<<
airlines <- read.csv(file.choose())
View(airlines)
#Scaling down the data to -1 to 1  
norm_air = scale(airlines[,2:12])
#Creating an instance fit having K means Algorithm for clustering with clusters 3
fit<- kmeans(norm_air,3)
str(fit)
#creating the dataframe and applying the Kmeans algorithm.
final<- data.frame(airlines, fit$cluster) # append cluster membership
final
final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
aggregate(airlines[,2:12], by=list(fit$cluster), FUN=mean)

#Calculating the significant value for Number of clusters and value for total withinn sum oof squares.
twss = c()
for (i in 1:15) twss[i] = sum(kmeans(norm_air, centers=i)$withinss)
plot(1:15, twss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
View(twss)
km <- kmeans(airlines,5) #kmeans clustering
str(km)
km$withinss
#K Means Clustering the data using the CLARA method 
library(cluster)
xds <- rbind(cbind(rnorm(airlines), rnorm(airlines)), cbind(rnorm(airlines), rnorm(airlines)))
xcl <- clara(xds, 5, sample = 1000)
clusplot(xcl)#Plotting the diiffrent clusters formed and their distances.
