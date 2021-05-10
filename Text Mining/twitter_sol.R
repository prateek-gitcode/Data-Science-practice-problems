library("twitteR")
library("ROAuth")
cred <- OAuthFactory$new(consumerKey='43hHJkZa4snEM8vbikYur2C0V', # Consumer Key (API Key)
                         consumerSecret='T3nKt2VN2qtjMc3sHDRumSXfTuxSIDPbGIToFbBa2kFtgx7BKM', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

install.packages("base64enc")
library(base64enc)

install.packages("httpuv")
library(httpuv)

setup_twitter_oauth("43hHJkZa4snEM8vbikYur2C0V", # Consumer Key (API Key)
                    "T3nKt2VN2qtjMc3sHDRumSXfTuxSIDPbGIToFbBa2kFtgx7BKM", #Consumer Secret (API Secret)
                    "529590041-AJNiB9OMFcaZuCqzoN99fH4pyfUN8uJ9EXDBwY5B",  # Access Token
                    "frz56qjS1TXT1jmBcIWEmXGFLXQu1X3gUswyvMEdCMOfj")  #Access Token Secret

Tweets <- userTimeline('kunalkamra88', n = 1000,includeRts = T)

TweetsDF <- twListToDF(Tweets)
write.csv(TweetsDF, "Tweets.csv")

library(syuzhet)
tweets_review <- read.csv(file.choose())

library(tm)
s_v <- vector()

for ( i in 1:200)
  s_v[i] <- as.character(tweets_review[i,2])

s_v <- removeNumbers(s_v)
s_v <- removePunctuation(s_v)
s_v <- removeWords(s_v,stopwords("english"))
s_v <- stripWhitespace(s_v)

#check the data for clarification

class(s_v)
str(s_v)
head(s_v)

sentiment_vector <- get_sentiment(s_v,method = "bing")
head(sentiment_vector)

nrc_vector <- get_sentiment(s_v,method = "nrc")
head(nrc_vector)

sum(sentiment_vector)
mean(sentiment_vector)
summary(sentiment_vector)

#plot
plot(sentiment_vector,type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h= 0, col= "red")

#to extract the sentance with most negative emotional valence
negative <- s_v[which.min(sentiment_vector)]
negative

# and to extract most positive sentence
positive <- s_v[which.max(sentiment_vector)]
positive

#more depth
poa_v <- s_v
poa_sent <- get_sentiment(poa_v, method = "bing")
plot(poa_sent,type = "h", main = "LOTR using transformed Values",
     xlab = "Narrative Time", ylab = "Emotinal Valence")

#percentage based figures
percent_vals <- get_percentage_values(poa_sent)

plot(percent_vals,type = "l", main = "Throw thr ring in the volcano using percentsge based means",
     xlab = "Narrative Time", ylab = "Emotinal Valence", col="red")


ft_values <- get_transformed_values(poa_sent,
                                    low_pass_size = 3,
                                    x_reverse_len = 100,
                                    scale_vals = TRUE,
                                    scale_range = FALSE)

plot(ft_values, type = "h", main = "LOTR using Transformed values",
     xlab = "Narrative time", ylab = "Emotional Valence",
     col="red")

nrc_data <- get_nrc_sentiment(s_v)
nrc_score_sent <- get_nrc_sentiment(negative)
nrc_score_word <- get_nrc_sentiment('grim')

#subset
sad_items <- which(nrc_data$sadness>0)
head(s_v[sad_items])

barplot(sort(colSums(prop.table(nrc_data[,1:8]))), horiz = T, cex.names = 0.7,
        las= 1, main = "Emotions", xlab="Percentage",col = 1:8)