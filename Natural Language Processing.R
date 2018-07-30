#Natural Language Processing

dataset_original=read.delim('Restaurant_Reviews.tsv',header = TRUE,sep = '\t',quote = '',stringsAsFactors = FALSE)

#Clean the text
#install.packages('tm')
install.packages('SnowballC')
library(SnowballC)
library(tm)
corpus=VCorpus(VectorSource(dataset$Review))
corpus=tm_map(corpus,content_transformer(tolower))
corpus=tm_map(corpus,removeNumbers)
corpus=tm_map(corpus,removePunctuation)
corpus=tm_map(corpus,removeWords,stopwords())
corpus=tm_map(corpus,stemDocument)
corpus=tm_map(corpus,stripWhitespace)

#Creating bag of words
dtm=DocumentTermMatrix(corpus)
dtm=removeSparseTerms(dtm,0.999)
dataset=as.data.frame(as.matrix(dtm))
dataset$Liked=dataset_original$Liked
#Encoding target feature as factor
dataset$Liked= factor(dataset$Liked,levels = c(0,1))
#Splitting the dataset into training and test sets
library(caTools)
set.seed(123)
split=sample.split(dataset$Liked,SplitRatio = 0.8)
trainingset=subset(dataset,split==TRUE)
testset=subset(dataset,split==FALSE)
#Fitting Random Forest Classfier to the traning set
library(randomForest)
classifier=randomForest(x=trainingset[-692],y=trainingset$Liked,ntree = 10)
#Predicting the test set results
ypred=predict(classifier,newdata = testset[-692])
#Building confusion matrix
cmatrix=table(testset[,692],ypred)