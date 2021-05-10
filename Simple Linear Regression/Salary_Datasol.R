library(readxl)
library(ggplot2)
salary<- read.csv('Salary_Data.csv')
View(salary)
attach(salary)
#EDA
summary(salary)
plot(salary)
#Correlation co-efficient
cor(YearsExperience,Salary)
#SLR
reg<- lm(Salary~YearsExperience, data = salary)
summary(reg)
pred <- predict(reg)
pred
reg$residuals
mean(reg$residuals)
sqrt(mean(reg$residuals^2))
confint(reg,level=0.95)
predict(reg,interval="predict")
ggplot(data = salary, aes(x = YearsExperience, y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = salary, aes(x=YearsExperience, y=pred))

#Logarithmic model
plot(log(YearsExperience), Salary)
cor(log(YearsExperience), Salary)
reg_log <- lm(Salary ~ log(YearsExperience))
summary(reg_log)
predict(reg_log)
reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(salary))
confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")
ggplot(data = salary, aes(x = log(YearsExperience), y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = salary, aes(x=log(YearsExperience), y=pred))

# Exponential Model

plot(YearsExperience, log(Salary))
cor(YearsExperience, log(Salary))
reg_exp <- lm(log(Salary) ~ YearsExperience)
summary(reg_exp)
reg_exp$residuals
sqrt(mean(reg_exp$residuals^2))
log_s <- predict(reg_exp)
at <- exp(log_s)
error = salary$Salary - at
error
sqrt(sum(error^2)/nrow(salary))  #RMSE
confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

# Polynomial model with 2 degree (quadratic model)

plot(YearsExperience*YearsExperience, Salary)
cor(YearsExperience*YearsExperience, Salary)
plot(YearsExperience*YearsExperience, log(Salary))
cor(YearsExperience, log(Salary))
cor(YearsExperience*YearsExperience, log(Salary))
reg2degree <- lm(log(Salary) ~ YearsExperience + I(YearsExperience*YearsExperience))
summary(reg2degree)
logpol <- predict(reg2degree)
expy <- exp(logpol)
err = salary$Salary - expy
sqrt(sum(err^2)/nrow(salary))#RMSE 
confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")
ggplot(data = salary, aes(x = YearsExperience + I(YearsExperience^2), y = log(Salary))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = salary, aes(x=YearsExperience+I(YearsExperience^2), y=logpol))

#  Polynomial model with 3 degree

reg3degree<-lm(log(Salary)~YearsExperience + I(YearsExperience*YearsExperience) + I(YearsExperience*YearsExperience*YearsExperience))
summary(reg3degree)
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)
ggplot(data = salary, aes(x = YearsExperience + I(YearsExperience^2) + I(YearsExperience^3), y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = salary, aes(x=YearsExperience+I(YearsExperience^2)+I(YearsExperience^3), y=expy3))

#the Polynomial model with 3 degree model is the best among all models