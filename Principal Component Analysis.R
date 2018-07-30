#Principal Component Analysis
#Extracting the dataset into R
dataset=read.csv('Wine.csv')
#Splitting the dataset into training and test sets
library(caTools)
set.seed(123)
split=sample.split(dataset$Customer_Segment,SplitRatio = 0.8)
trainingset=subset(dataset,split==TRUE)
testset=subset(dataset,split==FALSE)
#Scaling the features
trainingset[-14]=scale(trainingset[-14])
testset[-14]=scale(testset[-14])
#Applying PCA
#install.packages('caret')
library(caret)
pca=preProcess(x=trainingset[-14], method = 'pca', pcaComp = 2)
trainingset=predict(pca,trainingset)
trainingset=trainingset[c(2,3,1)]
testset=predict(pca,testset)
testset=testset[c(2,3,1)]
library(e1071)
classifier=svm(formula=Customer_Segment~.,data=trainingset,type='C-classification',kernel='linear')
#Predicting the test set results
ypred=predict(classifier,type='response',newdata = testset[-3])
#Building confusion matrix
cmatrix=table(testset[,3],ypred)
# Visualising the Training set results
library(ElemStatLearn)
set = trainingset
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
gridSet = expand.grid(X1, X2)
colnames(gridSet) = c('PC1', 'PC2')
probSet = predict(classifier, type = 'response', newdata = gridSet)
yGrid = predict(classifier,newdata = gridSet)
plot(set[, -3],main = 'SVM Classfier (Training set)',xlab = 'PC1', ylab = 'PC2',xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(yGrid), length(X1), length(X2)), add = TRUE)
points(gridSet, pch = '.', col = ifelse(yGrid==2,'deepskyblue', ifelse(yGrid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2,'blue3', ifelse(set[, 3] == 1, 'green', 'orange')))
# Visualising the Test set results
set = testset
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
gridSet = expand.grid(X1, X2)
colnames(gridSet) = c('PC1', 'PC2')
probSet = predict(classifier, type = 'response', newdata = gridSet)
yGrid = predict(classifier,newdata = gridSet)
plot(set[, -3],main = 'SVM Classifier(Test set)',xlab = 'PC1', ylab = 'PC2',xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(yGrid), length(X1), length(X2)), add = TRUE)
points(gridSet, pch = '.', col = ifelse(yGrid==2,'deepskyblue',ifelse(yGrid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2,'blue3', ifelse(set[, 3] == 1, 'green', 'orange')))