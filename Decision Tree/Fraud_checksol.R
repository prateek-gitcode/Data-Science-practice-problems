#Importing the dataset
library(readr)
fraud_data <- read_csv(file.choose())
View(fraud_data)
summary(fraud_data)#EDA

#Encoding target feature as factor
fraud_data$Fraud <- factor(ifelse(fraud_data$Taxable.Income>30000,"Good","Risky"))

#Splitting the dataset into training set and test set
library(caTools)
split <- sample.split(fraud_data$Fraud,SplitRatio =0.80)
train_data <- subset(fraud_data,split==TRUE)
test_data <- subset(fraud_data,split==FALSE)

#Preparing the model
library(rpart)
classifier <- rpart(formula = Fraud~.,data=train_data)
summary(classifier)
y_pred <- predict(classifier,newdata = test_data[-7],type='class')
library(gmodels)
CrossTable(test_data$Fraud,y_pred)
mean(test_data$Fraud==y_pred)

# Plotting the tree
plot(classifier)
text(classifier)
