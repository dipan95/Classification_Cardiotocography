#installing Packages required for classification
install.packages("caret")
install.packages("mlbench")
install.packages("neuralnet") 
install.packages("Metrics")

#Loading The packages 
library(ggplot2)
library(lattice)
library(caret)
library(mlbench)
library(neuralnet)
library(Metrics)

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


#Converting into factors
training$normal = training$NSP == "1"
training$suspect = training$NSP == "2"
training$pathological = training$NSP == "3"

nn = neuralnet(normal + suspect + pathological~A+B+D+E+AD+DE+LD+FS+SUSP+CLASS, training, hidden=3)
plot(nn)

mypredict <- compute(nn,cd[-5])$net.result

maxidx <- function(arr) 
  return(which(arr == max(arr)))

idx <- apply(mypredict, c(1), maxidx) 
print(idx)
prediction <- c("1", "2", "3")[idx]
pred<-table(prediction, idx)
accuracy = (sum(diag(pred))/sum(pred))
accuracy*100
#accuracy(prediction,cd$NSP)


