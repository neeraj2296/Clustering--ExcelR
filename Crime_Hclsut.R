#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Importing the data set <<<<<<<<<<<<<<<<<<<<<
crime <- read.csv(file.choose())
summary(crime)
#Scaling down the data to -1 to 1  
norm_data = scale(crime[,2:5])
summary(norm_data)
#calculating the euclidean distance between the data points of the data set
dist = dist(norm_data, method = "euclidean")
#Applying Heirarchical Clustering
fit <- hclust(dist, method = "complete")
#Applying Heirarchical Clustering
#Plotting the dendrogram
plot(fit)
plot(fit,hang = -1)
#Cutting the dendrogram into k number of clusters
group <- cutree(fit, k =4)
rect.hclust(fit, k=4, border="red")
#Conacatinating the respective groups classification of data points to the data set
places<-as.matrix(group)
final <- data.frame(crime,places)
final1 <- final[,c(ncol(final),1:(ncol(final)-1))]

#editing & creating a new data set groups classified. 
aggregate(crime[,2:4], by=list(final$places), FUN=mean)
write.csv(final1, file="final.csv")
#finale <- read.csv(file.choose())
