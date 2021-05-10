#Importing the Dataset
library(readr)
forest_fire <- read_csv(file.choose()) 
forest_fire <- forest_fire[3:31]

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

#Encoding target variable as factor
forest_fire$size_category <- factor(forest_fire$size_category, levels = c('small','large'),labels = c(1,2))

#Splitting the data into training set and test set
library(caTools)
set.seed(123)
split <- sample.split(forest_fire,SplitRatio = 0.8)
training_data <- subset(forest_fire,split==TRUE)
test_data <- subset(forest_fire,split==FALSE)

#Applying neural net model
install.packages('nnet')
library(nnet)
classifier <- nnet(formula = size_category~.,data = training_data,size=5)

# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate output for the model prepared

model_result <- predict(classifier,test_data,type = 'class')

#confusion matrix
library(gmodels)
CrossTable(model_result,test_data$size_category)
accuracy <- mean(model_result==test_data$size_category)
cor(as.numeric(factor(model_result)),as.numeric(test_data$size_category))
