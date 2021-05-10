#Importing the dataset
library(readr)
glass <- read_csv(file.choose())
View(glass)
summary(glass)

#Splitting the data into training set and test set
library(caTools)
split <- sample.split(glass$Type,SplitRatio = 0.8)
training_set <- subset(glass,split==TRUE)
test_set <- subset(glass,split==FALSE)
test_acc <- NULL
ki <- NULL

#Implementing KNN Algorithm
library(class)
for (i in seq(3,171,2))
{
  classifier <-knn(train = training_set[-10],test=test_set[-10],cl=training_set$Type,k=i,prob=TRUE)
  ki <- c(ki,i)
  test_acc <- c(test_acc,mean(test_set$Type==classifier))
}

k1=match(max(test_acc),test_acc)

#Accuracy when k=5
y_pred <-knn(train = training_set[-10],test=test_set[-10],cl=training_set$Type,k=5,prob = TRUE)
library(gmodels)
CrossTable(test_set$Type,y_pred)
mean(test_set$Type==y_pred)*100 
