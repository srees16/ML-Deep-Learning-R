#Polynomial Linear Regression
#Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset= dataset[2:3]

#Fitting linear regression to the dataset
linreg=lm(formula=Salary~.,
          data=dataset)
#Fittin polynomial linear regression to the dataset
dataset$Level2=dataset$Level^2
dataset$Level3=dataset$Level^3
dataset$Level4=dataset$Level^4
polyreg=lm(formula=Salary ~ .,
           data=dataset)

#Visualizing linear regression results
library(ggplot2)
ggplot()+
  geom_point(aes(x=dataset$Level,y=dataset$Salary),
             colour='red')+
  geom_line(aes(x=dataset$Level,y=predict(linreg, newdata=dataset)),
            colour='blue')+
  ggtitle('Truth Or Bullshit(LR)')+
  xlab('Level')+
  ylab('Salary')

#Visualizing polynomial regression results
ggplot()+
  geom_point(aes(x=dataset$Level,y=dataset$Salary),
             colour='red')+
  geom_line(aes(x=dataset$Level,y=predict(polyreg, newdata=dataset)),
            colour='blue')+
  ggtitle('Truth Or Bullshit(PR)')+
  xlab('Level')+
  ylab('Salary')

#Predicting a new result with linear regression
ypred=predict(linreg,data.frame(Level=6.5))

#Predicting a new result with polynomial regression
ypred=predict(polyreg,data.frame(Level=6.5,
                                 Level2=6.5^2,
                                 Level3=6.3^3,
                                 Level4=6.5^4))