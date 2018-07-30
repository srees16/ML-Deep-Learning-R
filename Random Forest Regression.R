#Random Forest Regression

#Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset= dataset[2:3]

#Fitting Random Forest Regression to the dataset
#Create regressor here
install.packages('randomForest')
library(randomForest)
set.seed(1234)
regressor=randomForest(x = dataset[1],
                       y = dataset$Salary,
                       ntree = 500)

#Predicting a new result with polynomial regression
ypred=predict(regressor,data.frame(Level=6.5))

#Visualizing Random Forest Regression results (smoother high res curve)
library(ggplot2)
xgrid=seq(min(dataset$Level),max(dataset$Level),0.01)
ggplot()+
  geom_point(aes(x=dataset$Level,y=dataset$Salary),
             colour='red')+
  geom_line(aes(x=xgrid,y=predict(regressor, newdata=data.frame(Level=xgrid))),
colour='blue')+
  ggtitle('Truth Or Bullshit(Random Forest Regression Model)')+
  xlab('Level')+
  ylab('Salary')