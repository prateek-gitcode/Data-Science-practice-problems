library(readr)
crime_data <- read_csv(file.choose())
View(crime_data)

#elbow Curve & k ~ sqrt(n/2) to decide K value
normalized_data=scale(crime_data[,2:5])

wss=(nrow(normalized_data)-1)*sum(apply(normalized_data,2,var))

for (i in 2:10)
  wss[i]=sum(kmeans(normalized_data,centers = i)$withinss)
plot(1:10,wss,type='b',xlab='Number of Clusters',ylab='Within Sum  of Squares')
title(sub='K-Means Clustering Screen-plot')

km <- kmeans(normalized_data,3)
str(km)

km1 <- kmeans(normalized_data,4)
str(km1)

#Among them 4 is appropriate
km1$cluster
final2 <- data.frame(crime_data,km1$cluster)
View(final2)
library(data.table)
setcolorder(final2,neworder = c("km1.cluster","Assault"))
View(final2)
aggregate(crime_data[,2:5],by=list(km1$cluster),FUN = mean)
km1$size

#Hierarchical Clustering
dendogram <- hclust(d=dist(normalized_data,method = 'euclidean'),method = 'complete')
plot(dendogram,hang=-1)
#Fitting the  clustree
hc <- hclust(d=dist(normalized_data,method = 'euclidean'),method = 'complete')
y_hc <- cutree(hc,2)
final3 <- data.frame(crime_data,y_hc)
library(cluster)
clusplot(normalized_data,y_hc)
