#Importing the dataset
dataset=read.csv('Data.csv')

#Splitting data into test set and training set
#install.packages('caTools')
library(caTools)
set.seed(123)
split=sample.split(dataset$Purchased,SplitRatio = 0.8)
trainingset=subset(dataset,split==TRUE)
testset=subset(dataset,split==FALSE)

#Feature Scaling
#trainingset[,2:3]=scale(trainingset[,2:3])
#testset[,2:3]=scale(testset[,2:3])