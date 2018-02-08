# import libraries we need
library(DBI)
library(RPostgreSQL)
library(Matrix)
library(arules)
library(grid)
library(arulesViz)

#set working dir
setwd("~/Documents/iems308")

# set random seed

set.seed(212)

# connect to database and get the entire data

con = dbConnect(PostgreSQL(),user = 'slg8734', password = 'slg8734_pw', host ='gallery.iems.northwestern.edu', dbname='iems308')
data<- dbGetQuery(con, "select * from pos.trnsact limit 200000")

# look the data features and select the subset of the data
data<- na.omit(data)
head(data, n=10)
df<- data[,c(1,2,3,4,6)]
colnames(df)<- c('SKU','Store','Register','Trannum','Saledate')

# modify the data for assoication rule analysis

df$SKU <- as.factor(df$SKU)
df$Store<- as.numeric(df$Store)
df$Register<- as.numeric(df$Register)
df$Trannum<- as.numeric(df$Trannum)
df$Saledate<- as.Date(df$Saledate)
df$basket<- paste(df$Store,df$Register,df$Trannum,df$Saledate,collapse = NULL, sep = ',')
df$basket<- as.factor(df$basket)
tran<- data.frame(df$basket,df$SKU)

#write the data into transaction csv file
transaction<- write.csv(tran,file = 'transaction.csv',row.names = FALSE)
transaction<- read.transactions('transaction.csv',cols = c(1,2), format = 'single',rm.duplicates = TRUE)

# use the aproioi to get assoication rules
rules<- apriori(transaction, parameter = list(supp=0.000017,conf=0.015,minlen=2))
rules<- sort(rules,by='lift')
inspect(rules)
summary(rules)

#find redundant rules 
subset.matrix<-is.subset(rules,rules,sparse = FALSE)
subset.matrix[lower.tri(subset.matrix,diag = T)]<-NA
redundant<-colSums(subset.matrix,na.rm=T)>=1
which(redundant)

# clean redundant rules
rules.pruned<- rules[!redundant]
inspect(rules.pruned)

#rules visualization 
plot(rules.pruned)
plot(rules.pruned,method="graph",control=list(type="items"))
