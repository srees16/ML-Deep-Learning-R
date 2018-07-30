#Hierarchical Clustering
dataset=read.csv('Mall_Customers.csv')
x=dataset[4:5]
#Dendogram to find optimal no of clusters
dendogram=hclust(dist(x,method = 'euclidean'),method = 'ward.D')
plot(dendogram,main = paste('Dendogram'),xlab = 'Customers',ylab = 'Euclidean Distances')
#Fitting the Hierarchical cluster to the dataset
hc=hclust(dist(x,method = 'euclidean'),method = 'ward.D')
yHc=cutree(hc,5,)
#Visualizing the H.Clustering
library(cluster)
clusplot(x,yHc,lines = 0,shade = FALSE,color = TRUE,labels = 2,plotchar = FALSE,
span = TRUE,main = paste('Clusters of Clients'),xlab = 'Annual Income',ylab='Spending Score')