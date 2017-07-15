##About this code-----
###Prediction task is to determine whether a person makes 
##over 50K a year: Low income & High Income person-----###
#install and load necessary packages
#install.packages("rpart.plot")
#install.packages("rattle",dependencies = TRUE)
library(rpart.plot)
library(rpart)
library(rattle)
library(dplyr)
library(data.table)
library(sqldf)
library(tree)
#creating URL variables containing train, test and readme files
url.train <-"http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
url.test <- "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test"
url.names <- "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names"
#loading data into variables from URL links
download.file(url.train, destfile = "Adult_train.csv")
download.file(url.test, destfile = "Adult_test.csv")
download.file(url.names, destfile = "Adult_Income_Readme.txt")
##Creatingtrain and test data----
train <- read.csv("Adult_train.csv", header = FALSE)
test <- read.csv("Adult_test.csv",header = FALSE)
head(test)
####EDA-----
#test set contains unwanteds 1st row ,which need to be dropped
test <- test[-1,]
#str(train)
#str(test)
# removing unwanted column value records
train1=train
train2=sqldf("select * from train1 where V2 NOT like '%?%' and V7 NOT like '%?%'
             and V14 NOT like '%?%'")

write.csv(train2,"train_clean.csv")
train2=read.csv("train_clean.csv")
str(train2)
#DROP ROW NUMBER COLUMN
train2<-train2[,-1]
# Renaming the column names from the readme.txt
varnames=c("AGE",
     "WORKCLASS",
    "FNLWGT",
    "EDUCATION",
    "EDUCATION_NUM",
    "MARITAL_STATUS",
    "OCCUPATION",
    "RELATIONSHIP",
    "RACE",
    "SEX",
    "CAPITAL_GAIN",
    "CAPITAL_LOSS",
    "HOURS_PER_WEEK",
    "NATIVE_COUNTRY",
    "INCOME_LEVEL")
names(train2)<-varnames
# preparing the test data
#str(test)
names(test)<-varnames
# removing unwanted symbols from test set
test1=sqldf("select * from test where WORKCLASS NOT like '%?%' 
	     and EDUCATION NOT like '%?%'
           and MARITAL_STATUS NOT like '%?%' 
           and OCCUPATION NOT like '%?%' 
           and RELATIONSHIP NOT like '%?%'
           and RACE NOT like '%?%' 
           and SEX NOT like '%?%' 
           and NATIVE_COUNTRY NOT like '%?%' 
           and INCOME_LEVEL NOT like '%?%' ")
 
#export n import to fix the variable levels memory issue
write.csv(test1,"test_clean.csv")
test2=read.csv("test_clean.csv")
str(test2)
test2<-test2[,-1]
#Matching the income_level variable levels 
levels(test2$INCOME_LEVEL)<-levels(train2$INCOME_LEVEL)
#1: Build the decision Tree using rpart package
dtree<-rpart(INCOME_LEVEL~.,data=train2,method ="class")
plot(dtree)
text(dtree,pretty = 0)
fancyRpartPlot(dtree, main = "Adult Income Level")
print(dtree)
#preictions on test data
test_pred=predict(dtree,test2[,1:14])
str(test_pred)
results=ifelse(test_pred[,1] >=0.5," <=50K"," >50K")
#model accuracy
accuracy<-round(sum(results==test2[,15])/length(results),digit=4)
print(paste("The model correctly predicted the test outcome ",
            accuracy*100, "% of the time", sep="")) # 83.4%
##2: Building the model using tree package
tree1 <- tree(INCOME_LEVEL~.-NATIVE_COUNTRY,data=train2)
str(tree1)
str(tree.pred)
tree.pred=predict(tree1,test2,type="class")
table(tree.pred,test2$INCOME_LEVEL)
mean(tree.pred==test2$INCOME_LEVEL) # 0.8389774
