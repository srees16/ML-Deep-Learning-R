#Support Vector Regression
#Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset= dataset[2:3]

#Fitting Support Vector Regression to the dataset
install.packages('e1071')
library(e1071)
regressor = svm(formula=Salary ~ ., data = dataset, type= 'eps-regression')

#Predicting a new result
ypred=predict(regressor,data.frame(Level=6.5))

#Visualizing Support Vector Regression results
library(ggplot2)
ggplot()+
  geom_point(aes(x=dataset$Level,y=dataset$Salary),
             colour='red')+
  geom_line(aes(x=dataset$Level,y=predict(regressor, newdata=dataset)),
            colour='blue')+
  ggtitle('Truth Or Bullshit(Support Vector Regression)')+
  xlab('Level')+
  ylab('Salary')