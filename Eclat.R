#Eclat
library(arules)
dataset=read.csv('Market_Basket_Optimisation.csv',header = FALSE)
dataset=read.transactions('Market_Basket_Optimisation.csv',sep = ',',rm.duplicates = TRUE)
summary(dataset)
itemFrequencyPlot(dataset,topN=10)
#Train Eclat model
rules=eclat(data=dataset,parameter = list(support=0.004, minlen=2))
#Visualizing th results
results=inspect(sort(rules,by='support')[1:10])