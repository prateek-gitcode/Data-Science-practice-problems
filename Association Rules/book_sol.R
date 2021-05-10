#BookData
#Importing the dataset
library(arules)
library(readr)
book <- read_csv(file.choose())
#Ruling upon data set
book_rules <- apriori(as.matrix(book),parameter = list(support=0.02,confidence=0.5,minlen=5))
#Changing Confidence,support, and minlen
book_rules_1 <- apriori(as.matrix(book),parameter = list(support=0.04,confidence=0.10,minlen=4))
quality(book_rules_1)= round(quality(book_rules_1), digits = 3)
rules.sorted= sort(book_rules_1, by="lift")
summary(book_rules)
#Finding Redundancy
redundant= is.redundant(book_rules,measure="confidence")
which(redundant)
rules.pruned= book_rules[!redundant]
rules.pruned= sort(rules.pruned, by="lift")
inspect(rules.pruned)
#Visualizing the results
inspect(sort(book_rules,by='lift')[1:10])
head(quality(book_rules))
library(arulesViz)
plot(book_rules,method='scatterplot')
plot(book_rules,method='grouped')
plot(book_rules,method='graph')