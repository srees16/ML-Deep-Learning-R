#Polynomial Regression Template
#Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset= dataset[2:3]

#Fitting polynomial regression to the dataset
#Create regressor here
dataset$Level2=dataset$Level^2
regressor=lm(formula=Salary ~ .,
             data=dataset)
#Predicting a new result with polynomial regression
ypred=predict(regressor,data.frame(Level=6.5,
                                   Level2=6.5^2))

#Visualizing polynomial regression results (smoother high res curve)
library(ggplot2)
xgrid=seq(min(dataset$Level),max(dataset$Level),0.1)
ggplot()+
  geom_point(aes(x=dataset$Level,y=dataset$Salary),
             colour='red')+
  geom_line(aes(x=xgrid,y=predictregressor, newdata=data.frame(Level=xgrid))),
colour='blue')+
  ggtitle('Truth Or Bullshit(Regression Model)')+
  xlab('Level')+
  ylab('Salary')