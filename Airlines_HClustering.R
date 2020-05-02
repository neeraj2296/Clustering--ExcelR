#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Importing the data set <<<<<<<<<<<<<<<<<<<<<
airlines<-read.csv(file.choose())#
summary(airlines)

#Scaling down the data to -1 to 1                           
norm_airlines<-scale(airlines[,2:12])
summary(norm_airlines)
#calculating the euclidean distance between the data points of the data set
dst<- dist(norm_airlines,method = "euclidean")
#Applying Heirarchical Clustering
fit<-hclust(dst,method = 'complete')
#Plotting the dendrogram
plot(fit)
plot(fit,hang = -1)
#Cutting the dendrogram into k number of clusters
grp <- cutree(fit,k=5)
rect.hclust(fit, k=5, border="red")
#Conacatinating the respective groups classification of data points to the data set
memb<-as.matrix(grp)
finale <- data.frame(airlines, memb)
finale1 <- finale[,c(ncol(finale),1:(ncol(finale)-1))]
#editing & creating a new data set groups classified. 
aggregate(airlines[,2:12], by=list(finale$memb), FUN=mean)
write.csv(finale1, file="finale.csv")
