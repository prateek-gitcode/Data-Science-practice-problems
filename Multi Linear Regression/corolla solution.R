#importing the dataset
library(readxl)
dataset= read.csv('ToyotaCorolla.csv')
corolla<- subset(dataset,select = c(Price,Age_08_04,KM,HP,cc,Doors,Gears,Quarterly_Tax,Weight))
View(corolla)
attach(corolla)
summary(corolla)#EDA

#Splitting the data into training set and test set
library(caTools)
set.seed(123)
split= sample.split(Price,SplitRatio = 0.8)
train_set= subset(corolla,split==TRUE)
test_set= subset(corolla,split==FALSE)
#Applying MLR to training set
regressor= lm(formula = Price~ .,data = train_set)
summary(regressor)
#Predicting the test set results
y_pred= predict(regressor,newdata = test_set)
y_pred
#preparing the model using backward elimination
regressor= lm(formula = Price~ Age_08_04 + KM + HP + cc + Doors + Gears + Quarterly_Tax + Weight,
              data = corolla)
summary(regressor)
regressor= lm(formula = Price~ Age_08_04 + KM + HP + cc + Gears + Quarterly_Tax + Weight,
              data = corolla)
summary(regressor)
regressor= lm(formula = Price~ Age_08_04 + KM + HP + Gears + Quarterly_Tax + Weight,
              data = corolla)
summary(regressor)
regressor= lm(formula = Price~ Age_08_04 + KM + HP + Gears + Weight,
              data = corolla)
summary(regressor)
regressor= lm(formula = Price~ Age_08_04 + KM + HP + Weight,
              data = corolla)
summary(regressor)
