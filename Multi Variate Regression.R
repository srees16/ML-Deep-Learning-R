#Muti Linear Regression

# Importing the dataset
dataset = read.csv('50_Startups.csv')

#Encoding categorical data
dataset$State=factor(dataset$State,
                       levels = c('New York','California','Florida'),
                       labels = c(1,2,3))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
trainingset = subset(dataset, split == TRUE)
testset = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

#Fitting Multi Variate Regression to the training set
regressor = lm(formula = Profit ~ .,
               data=trainingset)

#Predicting the test set results
yPred=predict(regressor,newdata = testset)

#Building Optimal model using Backward Elimination
regressor = lm(formula = Profit ~ R.D.Spend+Administration+Marketing.Spend+State,
               data=dataset)
summary(regressor)

regressor = lm(formula = Profit ~ R.D.Spend+Administration+Marketing.Spend,
               data=dataset)
summary(regressor)

regressor = lm(formula = Profit ~ R.D.Spend+Marketing.Spend,
               data=dataset)
summary(regressor)

regressor = lm(formula = Profit ~ R.D.Spend,
               data=dataset)
summary(regressor)

y_pred=predict(regressor,newdata = testset)

#Visualizing training set results
# install.packages('ggplot2')
library(ggplot2)
ggplot()+
  geom_point(aes(x=trainingset$R.D.Spend,y=trainingset$Profit),
             colour='red')+
  geom_line(aes(x=trainingset$R.D.Spend,y=predict(regressor,newdata = trainingset)),
            colour='blue')+
  ggtitle('R.D.Spend vs Profit (Training Set)')+
  xlab('R.D.Spend')+
  ylab('Profit')

#Visualizing test set results
# install.packages('ggplot2')
library(ggplot2)
ggplot()+
  geom_point(aes(x=testset$R.D.Spend,y=testset$Profit),
             colour='red')+
  geom_line(aes(x=trainingset$R.D.Spend,y=predict(regressor,newdata = trainingset)),
            colour='blue')+
  ggtitle('R.D.Spend vs Profie (Test Set)')+
  xlab('R.D.Spend')+
  ylab('Profit')