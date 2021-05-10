#Importing the dataset
library(readr)
forest_fires <- read_csv(file.choose())
forest_fires <- forest_fires[-c(1:2)]
forest_fires$size_category <- factor(forest_fires$size_category)

#EDA
View(forest_fires)
summary(forest_fires[,1:8])
sum(is.na(forest_fires))
#Plot(Histogram and Feature Plots)
hist(forest_fires$FFMC)
hist(forest_fires$DMC)
hist(forest_fires$DC)
hist(forest_fires$ISI)
hist(forest_fires$temp)
hist(forest_fires$RH)
hist(forest_fires$wind)
hist(forest_fires$rain)
library(caret)
featurePlot(x=forest_fires[,1:8],y= forest_fires$size_category,plot="box",strip= strip.custom(par.strip.text= list(cex=.7)),
            scales=list(x=list(relation="free"),y=list(relation="free")))
#Splitting the data into training set and test set
library(caTools)
set.seed(123)
split <- sample.split(forest_fires$size_category,SplitRatio = 0.80)
training_data <- subset(forest_fires,split==TRUE)
test_data <- subset(forest_fires,split==FALSE)

#Applying SVM technique
library(kernlab)
classifier <- ksvm(size_category ~.,data=training_data,kernel = 'anovadot')
y_pred <- predict(classifier,test_data)
library(gmodels)
CrossTable(test_data$size_category,y_pred)
accuracy <- mean(y_pred==test_data$size_category)*100