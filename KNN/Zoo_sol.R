#Importing the Dataset
library(readr)
zoo_data <- read.csv(file.choose())
zoo_data <- zoo_data[-1]

#EDA
summary(zoo_data)

#Splitting the data into training set and test set
library(caTools)
split <- sample.split(zoo_data$type,SplitRatio = 0.75)
training_set <- subset(zoo_data,split==TRUE)
test_set <- subset(zoo_data,split==FALSE)

#Implementing KNN Algorithm
library(class)
train_acc <- NULL
test_acc <- NULL
for (i in seq(3,200,2))
{
  classifier_train <- knn(train=training_set[-17],test=test_set[-17],cl=training_set[,17],k=i)
  train_acc <-c(train_acc,mean(classifier_train==training_set[,17])) 
  
}

#This shows accuracy when k=3
y_pred <- knn(train = training_set[-17],test=test_set[-17],cl=training_set[,17],k=3)
library(gmodels)
CrossTable(test_set[,17],y_pred)
mean(y_pred==test_set[,17])*100
