#Implementing Decision Trees
install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")
install.packages("RGtk2")
library(tree)
library(rpart)
library(rpart.plot)
library(rattle)
library(dplyr)
library(data.table)
library(sqldf)
browseVignettes(package = "dplyr")
url.train <-"http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
url.test <- "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test"
url.names <- "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names"

download.file(url.train, destfile = "Adult_train.csv")
download.file(url.test, destfile = "Adult_test.csv")
download.file(url.names, destfile = "Adult_Income_Readme.txt")

#getwd()

train <- read.csv("Adult_train.csv", header = FALSE)
test <- read.csv("Adult_test.csv",header = FALSE)
head(test)
test <- test[-1,]
str(train)
str(test)
sum(is.na(train1$V2))

str(train)

train1=train

str(train1)
table(train1$V2)
str(train1$V2)
train1$V2[train1$V2 == " ?"] <- NA

#train2=as.data.frame(train1[train1 != " ?"])

train2=sqldf("SELECT * FROM train1 WHERE V2 NOT LIKE '%?%'")

str(train2)
train2=data.table()

summary(train1$V2)
sum(train1$v2 == " ?")
View(train1$V2)
View(train1)
# train2=subset(train1, V2 %in% c("Federal-gov"))
# summary(train2)


train1$V2=recode(train1$V2, ' ?'= NA)

train2$V2= lapply(train1$V2, FUN = function(zz) recode(zz, " ? = 'NA'"))

  
str(train1$V2)
sum(is.na(train1$V2))

str(test)




