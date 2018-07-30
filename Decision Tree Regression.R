#Decision Tree Regression
#Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset= dataset[2:3]

#Fitting Decision Tree Regression to the dataset
#install.packages('rpart')
library(rpart)
regressor=rpart(formula=Salary~.,
                data=dataset,
                control = rpart.control(minsplit = 1))

#Predicting a new result
ypred=predict(regressor,data.frame(Level=6.5))

#Visualizing polynomial regression results (smoother high res curve)
library(ggplot2)
xgrid=seq(min(dataset$Level),max(dataset$Level),0.01)
ggplot()+
  geom_point(aes(x=dataset$Level,y=dataset$Salary),
             colour='red')+
  geom_line(aes(x=xgrid,y=predict(regressor, newdata=data.frame(Level=xgrid))),
colour='blue')+
  ggtitle('Truth Or Bullshit(Decision Tree Regression)')+
  xlab('Level')+
  ylab('Salary')