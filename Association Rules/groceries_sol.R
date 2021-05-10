#Groceries dataSet
#Importing the dataset
install.packages('arules')
library(arules)
groceries <- read.csv(file.choose())
groceries_data <- read.transactions(file.choose(),header=FALSE,rm.duplicates = TRUE,format='basket')
inspect(groceries_data[1:10])
#Creating a sparse matrix
summary(groceries_data)
itemFrequencyPlot(groceries_data,topN=20)
#Ruling upon data set
groceries_rules <- apriori(groceries_data,parameter = list(support=0.006,confidence=0.05,minlen=3))
#Changing Confidence,support 
groceries_rules_1 <- apriori(groceries_data,parameter = list(support=0.002,confidence=0.10,minlen=3))
quality(groceries_rules_1)= round(quality(groceries_rules_1), digits = 3)
rules.sorted= sort(groceries_rules_1, by="lift")
summary(groceries_rules_1)
#Finding Redundancy
redundant= is.redundant(groceries_rules,measure="confidence")
which(redundant)
rules.pruned= groceries_rules[!redundant]
rules.pruned= sort(rules.pruned, by="lift")
inspect(rules.pruned)
#Visualising results
inspect(sort(groceries_rules,by='lift')[1:9])
library(arulesViz)
plot(groceries_rules,method='scatterplot')
plot(groceries_rules,method='grouped')
plot(groceries_rules,method='graph')
plot(groceries_rules,method="mosaic")