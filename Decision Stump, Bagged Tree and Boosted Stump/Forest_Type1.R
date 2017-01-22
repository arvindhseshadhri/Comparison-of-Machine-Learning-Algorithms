myData1 <-read.csv(file="/Users/ArvindhS/Classes/Machine Learning/Project/training.csv", header=TRUE, sep=",")
str(myData1)
myData2 <-read.csv(file="/Users/ArvindhS/Classes/Machine Learning/Project/testing.csv", header=TRUE, sep=",")
str(myData2)
library(caret)
set.seed(4909)
for (ratio in c(0.1, 0.2, 0.3, 0.4, 0.5)) {
ind<-createDataPartition(myData1$class, p = .8,list = FALSE,times = 1)
Traindata3 <- myData1[ ind,]
ind1<-createDataPartition(myData1$class, p = .2,list = FALSE,times = 1)
Testdata3  <- myData2[ ind1,]
library(RWeka)
library(datasets)
library(pROC)
stump <- DecisionStump(class~., data=Traindata3)
print(stump)
table(predict(stump, Traindata3), Traindata3$class)
testPred3 <- predict(stump, newdata=Testdata3)
result = table(testPred3, Testdata3$class);
cat("The Accuracy for",ratio,"Training is:")
accuracy4 = sum(sum(result * diag(4))) / sum(sum(result))
print(accuracy4)

boosting <- AdaBoostM1(class~., data=Traindata3)
table(predict(boosting, Traindata3), Traindata3$class)
testPred3 <- predict(boosting, newdata=Testdata3)
result = table(testPred3, Testdata3$class);
cat("The Accuracy for",ratio,"Training is:")
accuracy5 = sum(sum(result * diag(4))) / sum(sum(result))
print(accuracy5)

bagging <- Bagging(class~., data=Traindata3)
table(predict(bagging, Traindata3), Traindata3$class)
testPred3 <- predict(bagging, newdata=Testdata3)
result = table(testPred3, Testdata3$class);
cat("The Accuracy for",ratio,"Training is:")
accuracy6 = sum(sum(result * diag(4))) / sum(sum(result))
print(accuracy6)
}