#Importing the dataset
library(readr)
salary_data_train <- read_csv(file.choose())
salary_data_test <- read_csv(file.choose())
#EDA
summary(salary_data_train)

#Encoding target as factor
salary_data_train$Salary <- as.numeric(factor(salary_data_train$Salary))
salary_data_train$workclass <- as.numeric(factor(salary_data_train$workclass))
salary_data_train$education <- as.numeric(factor(salary_data_train$education))
salary_data_train$maritalstatus <- as.numeric(factor(salary_data_train$maritalstatus))
salary_data_train$occupation <- as.numeric(factor(salary_data_train$occupation))
salary_data_train$relationship <- as.numeric(factor(salary_data_train$relationship))
salary_data_train$race <- as.numeric(factor(salary_data_train$race))
salary_data_train$sex <- as.numeric(factor(salary_data_train$sex))
salary_data_train$native <- as.numeric(factor(salary_data_train$native))

salary_data_test$Salary <- as.numeric(factor(salary_data_test$Salary))
salary_data_test$workclass <- as.numeric(factor(salary_data_test$workclass))
salary_data_test$education <- as.numeric(factor(salary_data_test$education))
salary_data_test$maritalstatus <- as.numeric(factor(salary_data_test$maritalstatus))
salary_data_test$occupation <- as.numeric(factor(salary_data_test$occupation))
salary_data_test$relationship <- as.numeric(factor(salary_data_test$relationship))
salary_data_test$race <- as.numeric(factor(salary_data_test$race))
salary_data_test$sex <- as.numeric(factor(salary_data_test$sex))
salary_data_test$native <- as.numeric(factor(salary_data_test$native))

#Feature Scaling
salary_data_train[-14] <- scale(salary_data_train[-14])
salary_data_test[-14]<- scale(salary_data_test[-14])

install.packages('kernlab')
library(kernlab)
salary_classifier <- ksvm(Salary~.,data=salary_data_train,kernel='vanilladot')



#Predicting the test set results
y_pred <- predict(salary_classifier,newdata=salary_data_test[-14])

#Making Confusion Matrix 
cm=table(salary_data_test$Salary,y_pred)
library(gmodels)
CrossTable(y_pred,salary_data_test$Salary,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

mean(y_pred==salary_data_test$Salary)