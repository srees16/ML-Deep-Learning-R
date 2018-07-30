#Kernel PCA

dataset=read.csv('Social_Network_Ads.csv')
dataset=dataset[,3:5]
#Splitting the dataset into training and test sets
library(caTools)
set.seed(123)
split=sample.split(dataset$Purchased,SplitRatio = 0.75)
trainingset=subset(dataset,split==TRUE)
testset=subset(dataset,split==FALSE)
#Scaling the features
trainingset[,1:2]=scale(trainingset[,1:2])
testset[,1:2]=scale(testset[,1:2])
#Applying kernel PCA
library(kernlab)
kpca=kpca(~.,data=trainingset[-3],kernel='rbfdot',features=2)
trainingsetPCA=as.data.frame(predict(kpca,trainingset))
trainingsetPCA$Purchased=trainingset$Purchased
testsetPCA=as.data.frame(predict(kpca,testset))
testsetPCA$Purchased=testsetPCA$Purchased
#Fitting logistic regression to the traning set
classifier=glm(formula = Purchased~.,data = trainingsetPCA,family = binomial)
#Predicting the test set results
probablepredicts=predict(classifier,type='response',newdata = testsetPCA[-3])
ypred=ifelse(probablepredicts>0.5,1,0)
#Building confusion matrix
cmatrix=table(testset[,3],ypred)
# Visualising the Training set results
library(ElemStatLearn)
set = trainingset
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
gridSet = expand.grid(X1, X2)
colnames(gridSet) = c('V1', 'V2')
probSet = predict(classifier, type = 'response', newdata = gridSet)
yGrid = ifelse(probSet > 0.5, 1, 0)
plot(set[, -3],main = 'KPCA Logistic Regression (HIS Training set)',xlab = 'V1', ylab = 'V2',xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(yGrid), length(X1), length(X2)), add = TRUE)
points(gridSet, pch = '.', col = ifelse(yGrid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
# Visualising the Test set results
set = testset
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
gridSet = expand.grid(X1, X2)
colnames(gridSet) = c('V1', 'V2')
probSet = predict(classifier, type = 'response', newdata = gridSet)
yGrid = ifelse(probSet > 0.5, 1, 0)
plot(set[, -3],main = 'KPCA Logistic Regression (Test set)',xlab = 'V1', ylab = 'V2',xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(yGrid), length(X1), length(X2)), add = TRUE)
points(gridSet, pch = '.', col = ifelse(yGrid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))