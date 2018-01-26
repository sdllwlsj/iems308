#setting the working directory and importing important libraries
setwd("~/Documents/IEMS308/hw1")
library(data.table)
library(ggplot2)
library(dplyr)
library(cluster)

#importing data table
df<- fread("hw1.txt",sep = "\t")
df<- na.omit(df)
df1<- df[sample(nrow(df),1000000)]
names(df)
structure(df)
df2<- df1[,c(10,20,21,22,23,24,25,26)]
#


#adding new variable for our analysis
df2$total_submitted <- df2$line_srvc_cnt * df2$average_submitted_chrg_amt
df2$total_medicare_paid<- df2$line_srvc_cnt * df2$average_Medicare_payment_amt
df2$total_medicare_allowed<- df2$line_srvc_cnt * df2$average_Medicare_allowed_amt
 
#creating new table for our analysis
df3<- df2 %>% group_by(nppes_provider_city) %>%
  summarize(n = n(), avg_submitted = sum(total_submitted)/sum(line_srvc_cnt),
            avg_medicare_paid = sum(total_medicare_paid)/sum(line_srvc_cnt),
            avg_medicare_allowed = sum(total_medicare_allowed)/sum(line_srvc_cnt)) %>%
  mutate(Payment_Rate = avg_medicare_allowed/avg_submitted)

# painting histrogram to understand the distribution of variable and the exist of ouliers
par(mfrow=c(2,2))
ggplot(data = df3,aes(x = n)) + geom_histogram() + coord_trans(y="sqrt") + labs(x="Number of cities", y="frequency", title="frequency of cities")
ggplot(data = df3,aes(x = n, y = avg_submitted)) + geom_point(na.rm = TRUE, alpha = 1/3) + labs(x="Number of medicare serivces", y="avg_submitted")
ggplot(data = df3,aes(x = avg_submitted)) + geom_histogram() + coord_trans(y="sqrt") + labs(x="avg_submitted", y="frequency", title="amount of avg_submitted")
ggplot(data = df3,aes(x = avg_medicare_allowed)) + geom_histogram() + coord_trans(y="sqrt") + labs(x="avg_medicare_allowed", y="frequency", title="amount of avg_medicare_allowed")

#data standardlization
df3$nppes_provider_city = NULL
df3$avg_medicare_paid = NULL

df4<- as.matrix(df3)
cor(df4)
df4<- scale(df4)



#scree plot
wss<- (nrow(df4)-1) * sum(apply(df4,2,var))
for (i in 2:20) wss[i] = sum(kmeans(df4,centers = i)$withinss)
plot(1:20,wss,type = "b",xlab = "number of clusters", ylab = "SS within clusters")

#clustering
k_mean = kmeans(df4, 4)
plot(n~Payment_Rate,df4,col=k_mean$cluster)

k_mean$centers
k_mean$cluster
k_mean$size

#assessing the clustering quality

kms = silhouette(k_mean$cluster,dist = (df4))
summary(kms)
plot(kms)
