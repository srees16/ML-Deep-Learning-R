#XGBoost

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

#Fitting XGBoost to the training set
#install.packages('xgboost')
library(xgboost)
classifier=xgboost(data = as.matrix(trainingset[-11]),label = trainingset$Exited,nrounds = 10)

#Applying k-Fold Cross validation
library(caret)
library(e1071)
folds=createFolds(trainingset$Exited,k=10)
cv=lapply(folds, function(x) {
  trainfold=trainingset[-x,]
  testfold=trainingset[x,]
  classifier=xgboost(data = as.matrix(trainingset[-11]),label = trainingset$Exited,nrounds = 20)
  ypred=predict(classifier,newdata = as.matrix(testfold[-11]))
  ypred=(ypred>=0.5)
  cmatrix=table(testfold[,11],ypred)
  accuracy=(cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[1,2]+cm[2,1])
  return(accuracy)
})
accuracy= mean(as.numeric(cv))