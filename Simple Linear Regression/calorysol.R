library(readxl)
library(ggplot2)
calory<- read.csv('calories_consumed.csv')
View(calory)
summary(calory)
colnames(calory)<- c("Weight_gain","Calories_consumed")
attach(calory)
#EDA
summary(calory)
plot(Calories_consumed,Weight_gain)
#Correlation co-efficient
cor(Calories_consumed,Weight_gain)
#SLR
reg<- lm(Weight_gain~Calories_consumed,data = calory)
summary(reg)
pred<- predict(reg)
pred
reg$residuals
sqrt(mean(reg$residuals^2))
confint(reg,level = 0.95)
predict(reg,interval = 'predict')
ggplot(data = calory, aes(x = Calories_consumed, y = Weight_gain)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calory, aes(x=Calories_consumed, y=pred))
