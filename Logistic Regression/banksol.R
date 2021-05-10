#Importing the dataset
library(readr)
bank_full <- read_delim(file.choose(), 
                        ";", escape_double = FALSE, trim_ws = TRUE)
bank_full$y <- factor(bank_full$y,levels=c('no','yes'),labels=c(0,1))#Encoding target feature as factor
View(bank_full)
summary(bank_full)#EDA

#Splitting the data into training set and test set
library(caTools)
set.seed(123)
split <- sample.split(bank_full$y,SplitRatio = 0.75)
train_set <- subset(bank_full,split==TRUE)
test_set <- subset(bank_full,split==FALSE)

#Preparing the model
model <- glm(formula = y~.,data = train_set,family = 'binomial')
summary(model)
exp(coef(model))#Manual calculation of odds ratio

#Predicting the test set values
y_pred <- predict(model,newdata = test_set,type='response')
y_pred <- ifelse(y_pred>0.5,1,0)

#Confusion matrix
library(gmodels)
CrossTable(y_pred>0.5,test_set$y)
accuracy <- mean(y_pred==test_set$y)*100
library(ROCR)

rocrpred<-prediction(y_pred,test_set$y)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

str(rocrperf)#threshold values along with TPR and FPR in one data frame

rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)

rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))#Sorting TPR in the data frame in decreasing order

View(rocr_cutoff)
