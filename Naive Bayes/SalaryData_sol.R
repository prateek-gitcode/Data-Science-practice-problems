#Importing the dataset
library(readr)
salary_data_train <- read_csv(file.choose())
salary_data_test <- read_csv(file.choose())
#EDA
summary(salary_data_train)

#Encoding target feature as factor
salary_data_train$Salary <- factor(salary_data_train$Salary)

#Feature Scaling
salary_data_train[-14] <- scale(salary_data_train[-14])
salary_data_test[-14]<- scale(salary_data_test[-14])

#Applying the Naive Bayes algorithm to the training set
library(e1071)
salary_classifier <- naiveBayes(x=salary_data_train[-14],y=salary_data_train$Salary)


#Predicting test set results
y_pred <- predict(salary_classifier,newdata=salary_data_test[-14])

#Making Confusion Matrix 
cm=table(salary_data_test$Salary,y_pred)
library(gmodels)
CrossTable(y_pred,salary_data_test$Salary,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

mean(y_pred==salary_data_test$Salary)
#Creating another Naive Bayes model
salary_classifier_1 <- naiveBayes(x=salary_data_train[-14],y=salary_data_train$Salary,laplace = 1)

y_pred_new <- predict(salary_classifier_1,newdata=salary_data_test[-14])

#Making Confusion Matrix 
cm=table(salary_data_test$Salary,y_pred_new)
library(gmodels)
CrossTable(y_pred_new,salary_data_test$Salary,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

mean(y_pred_new==salary_data_test$Salary)
