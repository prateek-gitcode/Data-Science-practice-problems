#importing the dataset
library(readr)
fraud_data <- read_csv(file.choose())
View(fraud_data)
summary(fraud_data)#EDA

#Encoding categorical variables
fraud_data$Fraud <- factor(ifelse(fraud_data$Taxable.Income>30000,"Good","Risky"))
fraud_data$Marital.Status=factor(fraud_data$Marital.Status,levels = c('Single','Married','Divorced'),labels = c(1,2,3))
attach(fraud_data)

#Splitting the dataset into training set and test set
library(caTools)
split <- sample.split(fraud_data$Fraud,SplitRatio =0.80)
train_data <- subset(fraud_data,split==TRUE)
test_data <- subset(fraud_data,split==FALSE)

#Preparing the model
library(randomForest)
classifier <- randomForest(formula = Fraud ~ City.Population + Marital.Status + Taxable.Income+
                             factor(Undergrad)+Work.Experience,data=train_data,importance=TRUE,na.action = na.roughfix)
summary(classifier)
y_pred <- predict(classifier,newdata = test_data[-7],type='class')
library(gmodels)
CrossTable(test_data$Fraud,y_pred)
mean(test_data$Fraud==y_pred)

# Plotting the tree
varImpPlot(classifier)
