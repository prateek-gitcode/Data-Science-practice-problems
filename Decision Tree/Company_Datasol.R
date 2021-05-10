#Importing the dataset
library(readr)
company_data <- read_csv('Company_Data.csv')
View(company_data)
summary(company_data)#EDA

#Encoding the categorical variables
company_data$NewSales <- as.factor(ifelse(company_data$Sales>7.49,1,0))
company_data$Urban <- as.factor(company_data$Urban)
company_data$US <- as.factor(company_data$US)

#Splitting the dataset into training set and test set
library(caTools)
set.seed(123)
split <- sample.split(company_data$Sales,SplitRatio = 0.90)
train_set <- subset(company_data,split==TRUE)
test_set <- subset(company_data,split==FALSE)

#Preparing the model
library(rpart)
model <- rpart(formula = NewSales~.,data=train_set,method='class')
summary(model)
y_pred <- predict(model,newdata = test_set,type = 'class')
library(gmodels)
CrossTable(y_pred,test_set$NewSales)
accuracy <- mean(y_pred==test_set$NewSales)

#Building the tree
library(tree)
model_tree <- tree(formula = NewSales ~.,data=train_set)
summary(model_tree)
y_pred_tree <- predict(model_tree,newdata = test_set,type='class')
library(gmodels)
CrossTable(y_pred_tree,test_set$NewSales)
accuracy_tree <- mean(y_pred_tree==test_set$NewSales)
