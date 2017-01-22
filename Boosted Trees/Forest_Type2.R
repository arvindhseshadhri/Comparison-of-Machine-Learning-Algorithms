library(glmnet)
library(gbm)
library(rpart)
library(plyr)
myData1 <-read.csv(file="/Users/ArvindhS/Classes/Machine Learning/Project/training.csv", header=TRUE, sep=",")
str(myData1)
myData2 <-read.csv(file="/Users/ArvindhS/Classes/Machine Learning/Project/testing.csv", header=TRUE, sep=",")
str(myData2)
library(caret)
set.seed(4909)
for (ratio in c(0.5,0.6,0.7,0.8,0.9)) {
ind<-createDataPartition(myData1$class, p = ratio,list = FALSE,times = 1)
Traindata3 <- myData1[ ind,]
ind1<-createDataPartition(myData1$class, p = 1-ratio,list = FALSE,times = 1)
Testdata3  <- myData2[ ind1,]
library(RWeka)
library(datasets)
fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)

gbmFit1 <- train(class ~ ., data = Traindata3, method = "gbm",trControl = fitControl, verbose=FALSE)

testPred3 <- predict(gbmFit1, newdata=Testdata3)
result = table(testPred3, Testdata3$class);
cat("The Accuracy for",ratio,"Training is:")
accuracy4 = sum(sum(result * diag(4))) / sum(sum(result))
print(accuracy4)
}