#k Means Clustering

#Importing the data set
dataset=read.csv('Mall_Customers.csv')
x=dataset[4:5]

#Using elbow method to find optimal no of clusters
set.seed(6)
wcss=vector()
for(i in 1:10) wcss[i]= sum(kmeans(x,i)$withinss)
plot(1:10,wcss,type='b',main=paste('Clusters of clients',xlab='No of clusters',ylab='WCSS'))

#Fit k means to the dataset
set.seed(29)
kmeans=kmeans(x,5,iter.max = 300,nstart = 10)

#Plotting the clusters and visualize
library(cluster)
yKmeans=kmeans$cluster
clusplot(x,yKmeans,lines = 0,shade = FALSE,color = TRUE,labels = 2,plotchar = FALSE,
span = TRUE,main = paste('Clusters of Clients'),xlab = 'Annual Income',ylab='Spending Score')