library(readxl)
library(ggplot2)
delv<- read.csv('delivery_time.csv')
View(delv)
colnames(delv)<- c("Deliverytime","Sortingtime")
attach(delv)
#EDA
summary(delv)
plot(Sortingtime,Deliverytime)
#correlation co-efficient
cor(Sortingtime,Deliverytime)
#SLR
reg<- lm(Deliverytime~Sortingtime,data = delv)
summary(reg)
pred<- predict(reg)
pred
reg$residuals
sqrt(mean(reg$residuals^2))
confint(reg,level = 0.95)
predict(reg,interval = 'predict')
ggplot(data = delv, aes(x = Sortingtime, y = Deliverytime)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = delv, aes(x=Sortingtime, y=pred))

# x=log(sortingtime) y=deliverytime(Logarithmic model)

plot(log(Sortingtime),Deliverytime)

cor(log(Sortingtime),Deliverytime)

#Log Model
reg <- lm(Deliverytime ~ log(Sortingtime))
summary(reg)
ggplot(data = delv, aes(x = log(Sortingtime), y = Deliverytime)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = delv, aes(x=log(Sortingtime), y=pred))
#Exponential Model
sort_time=Sortingtime
X=(1+sort_time+sort_time*2)
plot(X,Deliverytime)
cor(X,Deliverytime)
reg <- lm(log(Deliverytime ) ~ X )
summary(reg)
