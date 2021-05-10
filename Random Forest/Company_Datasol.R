#importing the dataset
library(readr)
company_data <- read_csv(file.choose())
View(company_data)
summary(company_data)#EDA

#Encoding categorical variables
company_data$NewSales <- as.factor(ifelse(company_data$Sales>7.49,1,0))
company_data$Urban <- as.factor(company_data$Urban)
company_data$US <- as.factor(company_data$US)

#Splitting dataset into training set and test set
library(caTools)
set.seed(123)
split <- sample.split(company_data$Sales,SplitRatio = 0.90)
train_set <- subset(company_data,split==TRUE)
test_set <- subset(company_data,split==FALSE)

#Preparing the model
library(randomForest)
model <- randomForest(formula = NewSales~.,data=train_set,method='class')
summary(model)
y_pred <- predict(model,newdata = test_set,type = 'class')
library(gmodels)
CrossTable(y_pred,test_set$NewSales)
accuracy <- mean(y_pred==test_set$NewSales)

