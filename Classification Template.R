#Regression Template
#Extracting the dataset into R
dataset=read.csv('Social_Network_Ads.csv')
dataset=dataset[,3:5]
#Encoding the target feature as vector
dataset$Purchased= factor(dataset$Purchased,levels = c(0,1))
#Splitting the dataset into training and test sets
library(caTools)
set.seed(123)
split=sample.split(dataset$Purchased,SplitRatio = 0.75)
trainingset=subset(dataset,split==TRUE)
testset=subset(dataset,split==FALSE)
#Scaling the features
trainingset[,1:2]=scale(trainingset[,1:2])
testset[,1:2]=scale(testset[,1:2])
#Fitting Classfier to the traning set
classifier=??
#Predicting the test set results
probablepredicts=predict(classifier,type='response',newdata = testset[-3])
ypred=ifelse(probablepredicts>0.5,1,0)
#Building confusion matrix
cmatrix=table(testset[,3],ypred)

# Visualising the Training set results
library(ElemStatLearn)
set = trainingset
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
gridSet = expand.grid(X1, X2)
colnames(gridSet) = c('Age', 'EstimatedSalary')
probSet = predict(classifier, type = 'response', newdata = gridSet)
yGrid = ifelse(probSet > 0.5, 1, 0)
plot(set[, -3],main = 'Classfier (Training set)',xlab = 'Age', ylab = 'Estimated Salary',xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(yGrid), length(X1), length(X2)), add = TRUE)
points(gridSet, pch = '.', col = ifelse(yGrid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'blue', 'orange'))

# Visualising the Test set results
set = testset
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
gridSet = expand.grid(X1, X2)
colnames(gridSet) = c('Age', 'EstimatedSalary')
probSet = predict(classifier, type = 'response', newdata = gridSet)
yGrid = ifelse(probSet > 0.5, 1, 0)
plot(set[, -3],main = 'Classifier(Test set)',xlab = 'Age', ylab = 'Estimated Salary',xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(yGrid), length(X1), length(X2)), add = TRUE)
points(gridSet, pch = '.', col = ifelse(yGrid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'blue', 'orange'))