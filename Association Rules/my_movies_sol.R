#Movies
#Importing the Dataset
library(arules)
movies <- read.csv(file.choose())
#Ruling upon data set
movies_rules <- apriori(as.matrix(movies[-c(1:5)]),parameter = list(support=0.02,confidence=0.3,minlen=2))
#Changing Confidence,support  and minlen
movies_rules_1 <- apriori(as.matrix(movies[-c(1:5)]),parameter = list(support=0.03,confidence=0.4,minlen=3))
quality(movies_rules_1)= round(quality(movies_rules_1), digits = 3)
rules.sorted= sort(movies_rules_1, by="lift")
summary(movies_rules)
summary(movies_rules_1)
#Finding Redundancy
redundant= is.redundant(movies_rules,measure="confidence")
which(redundant)
rules.pruned= movies_rules[!redundant]
rules.pruned= sort(rules.pruned, by="lift")
inspect(rules.pruned)
#Visualizing the results
inspect(sort(movies_rules,by='lift')[1:10])
head(quality(movies_rules))
library(arulesViz)
plot(movies_rules,method='scatterplot')
plot(movies_rules,method='grouped')
plot(movies_rules,method='graph')