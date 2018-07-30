#Importing the dataset
dataset=read.csv('Data.csv')

#Filling up the missing data
dataset$Age = ifelse(is.na(dataset$Age),
                     ave(dataset$Age,FUN = function(x) mean (x,na.rm = TRUE)),
                     dataset$Age)

dataset$Salary = ifelse(is.na(dataset$Salary),
                     ave(dataset$Salary,FUN = function(x) mean (x,na.rm = TRUE)),
                     dataset$Salary)

#Encoding categorical data
dataset$Country=factor(dataset$Country,
                       levels = c('France','Spain','Germany'),
                       labels = c(1,2,3))

dataset$Purchased=factor(dataset$Purchased,
                       levels = c('Yes','No'),
                       labels = c(1,0))

#Splitting data into test set and training set
#install.packages('caTools')
library(caTools)
set.seed(123)
split=sample.split(dataset$Purchased,SplitRatio = 0.8)
trainingset=subset(dataset,split==TRUE)
testset=subset(dataset,split==FALSE)

#Feature Scaling
trainingset[,2:3]=scale(trainingset[,2:3])
testset[,2:3]=scale(testset[,2:3])