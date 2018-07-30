#Simple Linear Regression
#Importing the dataset
dataset=read.csv('Salary_Data.csv')

#Splitting data into test set and training set
#install.packages('caTools')
library(caTools)
set.seed(123)
split=sample.split(dataset$Salary,SplitRatio = 2/3)
trainingset=subset(dataset,split==TRUE)
testset=subset(dataset,split==FALSE)

#Fitting Simple Linear Regression to the training set
regressor= lm(formula = Salary ~ YearsExperience,
              data = trainingset)

#Predicting the test set results
y_pred=predict(regressor,newdata = testset)

#Visualizing training set results
# install.packages('ggplot2')
library(ggplot2)
ggplot()+
  geom_point(aes(x=trainingset$YearsExperience,y=trainingset$Salary),
             colour='red')+
  geom_line(aes(x=trainingset$YearsExperience,y=predict(regressor,newdata = trainingset)),
            colour='blue')+
  ggtitle('Salary vs Experience (Training Set)')+
  xlab('Years Of Experience')+
  ylab('Salary')

#Visualizing test set results
# install.packages('ggplot2')
library(ggplot2)
ggplot()+
  geom_point(aes(x=testset$YearsExperience,y=testset$Salary),
             colour='red')+
  geom_line(aes(x=trainingset$YearsExperience,y=predict(regressor,newdata = trainingset)),
            colour='blue')+
  ggtitle('Salary vs Experience (Test Set)')+
  xlab('Years Of Experience')+
  ylab('Salary')