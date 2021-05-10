library(readxl)
library(ggplot2)
emp<- read.csv('emp_data.csv')

#EDA
View(emp)
summary(emp)
attach(emp)
plot(Salary_hike,Churn_out_rate)

#Correlation Co-efficient
cor(Salary_hike,Churn_out_rate)

#SLR
reg<- lm(Churn_out_rate~Salary_hike, data = emp)
summary(reg)
pred <- predict(reg)
pred
reg$residuals
mean(reg$residuals)
sqrt(mean(reg$residuals^2))
confint(reg,level=0.95)
predict(reg,interval="predict")
ggplot(data = emp, aes(x = Salary_hike, y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp, aes(x=Salary_hike, y=pred))

#Logarithmic model
plot(log(Salary_hike), Churn_out_rate)
cor(log(Salary_hike), Churn_out_rate)
reg_log <- lm(Churn_out_rate ~ log(Salary_hike))
summary(reg_log)
predict(reg_log)
reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(emp))
confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")
ggplot(data = emp, aes(x = log(Salary_hike), y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp, aes(x=log(Salary_hike), y=pred))

# Exponential Model

plot(Salary_hike, log(Churn_out_rate))
cor(Salary_hike, log(Churn_out_rate))
reg_exp <- lm(log(Churn_out_rate) ~ Salary_hike)
summary(reg_exp)
reg_exp$residuals
sqrt(mean(reg_exp$residuals^2))
log_s <- predict(reg_exp)
at <- exp(log_s)
error = emp$Churn_out_rate - at
error
sqrt(sum(error^2)/nrow(emp))  #RMSE
confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

# Polynomial model with 2 degree (quadratic model)

plot(Salary_hike*Salary_hike, Churn_out_rate)
cor(Salary_hike*Salary_hike, Churn_out_rate)
plot(Salary_hike*Salary_hike, log(Churn_out_rate))
cor(Salary_hike, log(Churn_out_rate))
cor(Salary_hike*Salary_hike, log(Churn_out_rate))
reg2degree <- lm(log(Churn_out_rate) ~ Salary_hike + I(Salary_hike*Salary_hike))
summary(reg2degree)
logpol <- predict(reg2degree)
expy <- exp(logpol)
err = emp$Churn_out_rate - expy
sqrt(sum(err^2)/nrow(emp)) 
confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")
ggplot(data = emp, aes(x = Salary_hike + I(Salary_hike^2), y = log(Churn_out_rate))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp, aes(x=Salary_hike+I(Salary_hike^2), y=logpol))

#The Polynomial model with 2 degree (quadratic model) is the best model among all the above 