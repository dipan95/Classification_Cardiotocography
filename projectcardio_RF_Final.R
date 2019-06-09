#installing Packages required for classification
install.packages("caret")
install.packages("neuralnet") 
install.packages("Metrics")
install.packages("randomForest")
install.packages("party")
#Loading The packages 
library(ggplot2)
library(lattice)
library(caret)
library(neuralnet)
library(Metrics)

library(party)
library(randomForest)

#Reading Dataset(Filtered)
setwd("D:\\MLProject")
cd <- read.csv("CTGG2.csv",sep=',',header=TRUE)
str(cd)
head(cd)

cd$Date=NULL
cd$ï..FileName=NULL
cd$SegFile=NULL
cd$DR=NULL
cd$C=NULL
cd$Nzeros=NULL
cd$DS=NULL

#Dividing Dataset into Training and Testing
head(cd)
set.seed(3033)
intrain= sort(sample(nrow(cd),nrow(cd)*.7))
training <- cd[intrain,]
testing <-cd[-intrain,]
dim(training)
dim(testing)
anyNA(cdd)
summary(cdd)

rf = randomForest(factor(NSP) ~ .,  ntree = 100,data = training,importance = TRUE,ntree = 100,mtry = 6,keep.forest=TRUE)

imp = data.frame(importance(rf,type=2))
imp

pr = predict(rf , testing)
pr
k<-table(pr,testing$NSP)
accuracy = (sum(diag(k))/sum(k))
accuracy


#accuracy(pr,testing$NSP)
#plot(getTree(rf,1,labelVar = TRUE))

