#Linear Discriminant Analysis

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
#Applying LCA
#install.packages('MASS')
library(MASS)
lda=lda(formula=Customer_Segment~.,data=trainingset)
trainingset=as.data.frame(predict(lda,trainingset))
trainingset=trainingset[c(5,6,1)]
testset=as.data.frame(predict(lda,testset))
testset=testset[c(5,6,1)]
library(e1071)
classifier=svm(formula=class~.,data=trainingset,type='C-classification',kernel='linear')
#Predicting the test set results
ypred=predict(classifier,newdata = testset[-3])
#Building confusion matrix
cmatrix=table(testset[,3],ypred)
# Visualising the Training set results
library(ElemStatLearn)
set = trainingset
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
gridSet = expand.grid(X1, X2)
colnames(gridSet) = c('x.LD1', 'x.LD2')
probSet = predict(classifier, type = 'response', newdata = gridSet)
yGrid = predict(classifier,newdata = gridSet)
plot(set[, -3],main = 'SVM (LDA) Classfier (Training set)',xlab = 'x.LD1', ylab = 'x.LD2',xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(yGrid), length(X1), length(X2)), add = TRUE)
points(gridSet, pch = '.', col = ifelse(yGrid==2,'deepskyblue', ifelse(yGrid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2,'blue3', ifelse(set[, 3] == 1, 'green', 'orange')))
# Visualising the Test set results
set = testset
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
gridSet = expand.grid(X1, X2)
colnames(gridSet) = c('x.LD1', 'x.LD2')
probSet = predict(classifier, type = 'response', newdata = gridSet)
yGrid = predict(classifier,newdata = gridSet)
plot(set[, -3],main = 'SVM (LDA) Classifier(Test set)',xlab = 'x.LD1', ylab = 'x.LD2',xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(yGrid), length(X1), length(X2)), add = TRUE)
points(gridSet, pch = '.', col = ifelse(yGrid==2,'deepskyblue',ifelse(yGrid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2,'blue3', ifelse(set[, 3] == 1, 'green', 'orange')))