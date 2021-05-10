install.packages('gplots')
library(gplots)
library(readxl)
data<- read.csv('BuyerRatio.csv')
View(data)
attach(data)
rownames(data)<- data$Observed.Values
data$Observed.Values<- NULL
data2<- as.data.frame(t(as.matrix(data)))
data2
View(data2)
attach(data2)
shapiro.test(Males)
shapiro.test(Females)
table(Males,Females)
chisq.test(data2)
