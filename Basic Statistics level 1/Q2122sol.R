library(readxl)
data= read.csv('Cars.csv')
data2= read.csv('wc-at.csv')
attach(data)
attach(data2)
mean(MPG)
sd(MPG)
shapiro.test(MPG)
shapiro.test(Waist)
shapiro.test(AT)
pnorm(38,mean=34.42208,sd=9.131445,lower.tail = FALSE)#a.	P(MPG>38)#
pnorm(40,mean = 34.42208,sd = 9.131445,lower.tail = TRUE)#b.	P(MPG<40)#
pnorm(50, mean = 34.42208,sd = 9.131445, lower.tail = TRUE)-pnorm(20, mean = 34.42208,sd = 9.131445, lower.tail = TRUE)#c. P (20<MPG<50)#
