#Artificial Neural Network

#Extracting the dataset into R
dataset=read.csv('Churn_Modelling.csv')
dataset=dataset[4:14]
#Encoding the categorical variable as factors
dataset$Geography=as.numeric(factor(dataset$Geography,levels=c('France','Spain','Germany'),labels=c(1,2,3)))
dataset$Gender=as.numeric(factor(dataset$Gender,levels = c('Female','Male'),labels=c(1,2)))
#Splitting the dataset into training and test sets
library(caTools)
set.seed(123)
split=sample.split(dataset$Exited,SplitRatio = 0.8)
trainingset=subset(dataset,split==TRUE)
testset=subset(dataset,split==FALSE)
#Scaling the features
trainingset[-11]=scale(trainingset[-11])
testset[-11]=scale(testset[-11])
#Fitting Classfier to the training set
#install.packages('h2o')
library(h2o)
h2o.init(nthreads = -1)
classifier=h2o.deeplearning(y='Exited',training_frame = as.h2o(trainingset),activation = 'Rectifier',hidden = c(6,6),epochs = 100,train_samples_per_iteration = -2)
#Predicting the test set results
probablepredicts=h2o.predict(classifier,newdata = as.h2o(testset[-11]))
ypred=(probablepredicts>0.5)
ypred=as.vector(ypred)
#Building confusion matrix
cmatrix=table(testset[,11],ypred)
h2o.shutdown()