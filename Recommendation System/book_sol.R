library(readr)
library(caTools)
book_data <- read_csv(file.choose())
book_data <- book_data[-1]
colnames(book_data)[5] <- 'Ratings'
book_data$Book.Title <- as.numeric(factor(book_data$Book.Title))
book_data$Book.Author <- as.numeric(factor(book_data$Book.Author))
book_data$Publisher <- as.numeric(factor(book_data$Publisher))
book_data$Ratings <- as.numeric(factor(book_data$Ratings))
book_data$`users[, 1]` <- as.numeric(factor(book_data$`users[, 1]`))

book_data_new <- lapply(book_data, unlist)
book_data_new1 <- as(book_data_new,'data.frame')
str(book_data)
hist(book_data$Ratings)
library(recommenderlab)
#the datatype should be realRatingMatrix inorder to build recommendation engine
book_data_rate_matrix <- as(book_data_new1,'realRatingMatrix')

book_recommendation <- Recommender(book_data_rate_matrix,method='POPULAR')

#Predictions for two users 
recommended_items1 <- predict(book_recommendation, book_data_rate_matrix[413:414], n=5)
as(recommended_items1, "list")