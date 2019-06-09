#installing Packages required for classification
install.packages("caret")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("Boruta")

#Loading The packages
library(ranger)
library(Boruta)
library(ggplot2)
library(lattice)
library(caret)
library(rpart)

library(rpart.plot)
library(Metrics)


#Reading Dataset(Filtered)
setwd("D:\\MLProject")
cd <- read.csv("CTGG2.csv",sep=',',header=TRUE)
str(cd)
head(cd)

#applying Boruta to fing important attributes
set.seed(123)
boruta.train <- Boruta(NSP~., data =cd, doTrace = 2)
print(boruta.train)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

#removing unnecsaary attributes
cd$Date=NULL
cd$ï..FileName=NULL
cd$SegFile=NULL
cd$DR=NULL
cd$C=NULL
cd$Nzeros=NULL
cd$DS=NULL
head(cd)

#Dividing Dataset into Training and Testing
head(cd)
set.seed(3033)
intrain= sort(sample(nrow(cd),nrow(cd)*.7))
training <- cd[intrain,]
testing <-cd[-intrain,]
dim(training)
dim(testing)
anyNA(training)
summary(cdd)

#tree
trctrl <- trainControl(method="repeatedcv",number=10,repeats = 3)
set.seed(3333)
dtree_fit_gini <- train(NSP~., data=training,method="rpart",trControl=trctrl,tuneLength=10,parms=list(split="gini"))
dtree_fit_gini$finalModel
dtreepredict <- predict(dtree_fit_gini$finalModel,testing)
dtree_fit_giniprp <-prp(dtree_fit_gini$finalModel,box.palette="Blues",tweak=1.2)
accuracy(dtreepredict,testing$NSP)
