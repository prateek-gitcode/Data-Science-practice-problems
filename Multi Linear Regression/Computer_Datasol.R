#Importing the dataset
library(readr)
computer_data <- read_csv(file.choose())
computer_data <- computer_data[-1]
View(computer_data)
#EDA
summary(computer_data)


#Encoding the categorical data
computer_data$cd <- factor(computer_data$cd,levels = c('yes','no'),labels = c(1,2))
computer_data$multi<-factor(computer_data$multi,levels=c('yes','no'),labels=c(1,2))
computer_data$premium<-factor(computer_data$premium,levels=c('yes','no'),labels=c(1,2))

computer_data$cd <-  as.numeric(computer_data$cd)
computer_data$multi <- as.numeric(computer_data$multi)
computer_data$premium <-  as.numeric(computer_data$premium)
#Plotting the correlation coefficients
#Splitting the dataset into training set and test set
library(caTools)
set.seed(123)
split <- sample.split(computer_data$price,SplitRatio = 0.75)
train_Set <- subset(computer_data,split==TRUE)
test_set <- subset(computer_data,split==FALSE)

cor(train_Set)#correlation coefficient

#finding partial correlation
library(corpcor)
cor2pcor(cor(train_Set))

#Modeling of data taking every column in consideration

reg <- lm(price~.,data = train_Set)
summary(reg)
#Applying VIF function

vif(reg)
library(car)
influence.measures(reg)

influenceIndexPlot(reg,id.n=3)
influencePlot(reg,id.n=3)

reg1 <- lm(price ~.,data = train_Set[-c(2902,3426,1318,1107),])
summary(reg1)

#Predicting the test set results
y_pred <- predict(reg1,newdata=test_set)
error <- y_pred -test_set$price
RMSE <- sqrt(mean((y_pred -test_set$price)^2,na.rm = T))#error values
plot(reg1)
hist(residuals(reg1))
