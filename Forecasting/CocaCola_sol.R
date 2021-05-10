library(readxl)
cocasales<- read_xlsx(file.choose())
View(cocasales)

#PreProcessing
#Creating 12 dummy variables
Quaters <- c("Q1","Q2","Q3","Q4")
x <- data.frame(outer(rep(Quaters,length=42),Quaters,"==")+0)
colnames(x) <- Quaters
cocasales_data <- cbind(cocasales,x)

#input t
cocasales_data["t"] <-cbind(1:42) 
cocasales_data["log_Sales"] <- log(cocasales_data["Sales"])
cocasales_data["t_square"] <- cocasales_data["t"]*cocasales_data["t"]

#PreProcessing Completed
#Partitioning 
train_set <- cocasales_data[1:30,]
test_set <- cocasales_data[30:42,]

########################### LINEAR MODEL #############################


linear_model <- lm(Sales ~ t, data = train_set)
summary(linear_model)
linear_pred <- data.frame(predict(linear_model, interval='predict', newdata =test_set))
rmse_linear <- sqrt(mean((test_set$Sales-linear_pred$fit)^2, na.rm = T))
rmse_linear

######################### Exponential #################################

expo_model <- lm(log_Sales ~ t, data = train_set)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model, interval='predict', newdata = test_set))
rmse_expo <- sqrt(mean((test_set$Sales-exp(expo_pred$fit))^2, na.rm = T))
rmse_expo


######################### Quadratic ####################################
Quad_model <- lm(Sales ~ t+t_square, data = train_set)
summary(Quad_model)
Quad_pred <- data.frame(predict(Quad_model, interval='predict', newdata = test_set))
rmse_Quad <- sqrt(mean((test_set$Sales-Quad_pred$fit)^2, na.rm = T))
rmse_Quad
######################### Additive Seasonality #########################

sea_add_model <- lm(Sales ~ Q1+Q2+Q3, data = train_set)
summary(sea_add_model)
sea_add_pred <- data.frame(predict(sea_add_model, newdata=test_set, interval = 'predict'))
rmse_sea_add <- sqrt(mean((test_set$Sales-sea_add_pred$fit)^2, na.rm = T))
rmse_sea_add

######################## Additive Seasonality with Quadratic #################
Add_sea_Quad_model <- lm(Sales ~ t+t_square+Q1+Q2+Q3, data = train_set)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model, interval='predict', newdata = test_set))
rmse_Add_sea_Quad <- sqrt(mean((test_set$Sales-Add_sea_Quad_pred$fit)^2, na.rm = T))
rmse_Add_sea_Quad

######################## Multiplicative Seasonality #########################

multi_sea_model <- lm(log_Sales ~ +Q1+Q2+Q3, data = train_set)
summary(multi_sea_model)
multi_sea_pred <- data.frame(predict(multi_sea_model, newdata=test_set, interval='predict'))
rmse_multi_sea <- sqrt(mean((test_set$Sales-exp(multi_sea_pred$fit))^2, na.rm = T))
rmse_multi_sea


# Preparing table on model and it's RMSE values 

table_rmse <- data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea))
colnames(table_rmse) <- c("model","RMSE")
View(table_rmse)
# Exponential Model with Quadratic has least RMSE value

############### Combining Training & test data to build Additive seasonality using Quadratic Trend ############

expo_model_final <- lm(log_Sales ~t , data = cocasales_data)
summary(expo_model_final)

####################### Predicting new data #############################

pred_new <- data.frame(predict(expo_model_final,interval='predict',newdata=test_set))
plot(pred_new)
acf(expo_model_final$residuals,lag.max = 10)
A <- arima(expo_model_final$residuals,order=c(8,0,0))
acf(A$residuals,lag.max=10)

library(forecast)
errors_12 <- forecast(A,h=12)
future_errors <- data.frame(errors_12)$Point.Forecast
predicted_new_values <- pred_new+future_errors
