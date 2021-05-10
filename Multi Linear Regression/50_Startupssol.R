#Importing the dataset
install.packages("readxl")
library(readxl)
data= read.csv('50_Startups.csv')
#Encoding categorical variables
data$State<- factor(data$State,
                    levels = c('New York','California','Florida'),
                    labels = c(1,2,3))
View(data)
summary(data)#EDA
attach(data)

#Splitting the dataset into training and test set
library(caTools)
set.seed(123)
split= sample.split(Profit,SplitRatio = 0.8)
train_set= subset(data,split==TRUE)
test_set= subset(data,split==FALSE)

#Applying MLR to the training set 
regressor= lm(formula = Profit~ .,data = train_set)
summary(regressor)
#Predicting the test set results
y_pred= predict(regressor,newdata = test_set)
y_pred
#Finding the optimum model using backward elimination
regressor= lm(formula = Profit~ R.D.Spend + Administration + Marketing.Spend + State
              ,data = data)
summary(regressor)
regressor= lm(formula = Profit~ R.D.Spend + Administration + Marketing.Spend
              ,data = data)
summary(regressor)
regressor= lm(formula = Profit~ R.D.Spend + Marketing.Spend
              ,data = data)
summary(regressor)
regressor= lm(formula = Profit~ R.D.Spend
              ,data = data)
summary(regressor)
