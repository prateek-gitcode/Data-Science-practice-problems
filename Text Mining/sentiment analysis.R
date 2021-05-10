dataset= read.delim('negative-words.tsv', quote = '', stringsAsFactors = FALSE)
dataset= stack(dataset)
View(dataset)
install.packages('NLP')
install.packages('tm')
library(NLP)
library(tm)
corpus= VCorpus(VectorSource(dataset$values))
corpus= tm_map(corpus,content_transformer(tolower))
corpus= tm_map(corpus,removeNumbers)
corpus= tm_map(corpus,removePunctuation)
install.packages('SnowballC')
library(SnowballC)
corpus= tm_map(corpus,removeWords,stopwords())
corpus= tm_map(corpus,stemDocument)
corpus= tm_map(corpus,stripWhitespace)
dtm= DocumentTermMatrix(corpus)
dtm= removeSparseTerms(dtm,0.99)
dtm
install.packages("wordcloud")
install.packages("RColorBrewer")
library(wordcloud)
w <- sort(rowSums(dtm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.7)

