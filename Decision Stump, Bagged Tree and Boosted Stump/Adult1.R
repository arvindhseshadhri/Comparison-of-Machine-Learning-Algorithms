myData3 <-read.csv(file="/Users/ArvindhS/Classes/Machine Learning/Project/adult.csv", header=FALSE);
myData3<-na.omit(myData3)
str(myData3)
myData4 <-read.csv(file="/Users/ArvindhS/Classes/Machine Learning/Project/adult_test.csv", header=FALSE);
myData3<-na.omit(myData4)
str(myData4)
library(caret)
set.seed(4909)
for (ratio in c(0.1, 0.2, 0.3, 0.4, 0.5)) {
train_ind<-createDataPartition(myData3$V15, p = ratio,
                               list = FALSE,
                               times = 1)
Traindata3 <- myData3[ train_ind,]
train_ind1<-createDataPartition(myData4$V15, p = 1-ratio,
                                list = FALSE,
                                times = 1)
Testdata3  <- myData4[ -train_ind1,]
library(RWeka)
library(datasets)
library(pROC)
stump <- DecisionStump(V15~., data=Traindata3)
print(stump)
table(predict(stump, Traindata3), Traindata3$V15)
testPred3 <- predict(stump, newdata=Testdata3)
result = table(testPred3, Testdata3$V15);
cat("The Accuracy for",ratio,"Training is:")
accuracy4 = sum(sum(result * diag(2))) / sum(sum(result))
print(accuracy4)

boosting <- AdaBoostM1(V15~., data=Traindata3)
roc(as.factor(Testdata3$V15), predict(boosting, newdata = Testdata3, type = "probability")[, 2])
table(predict(boosting, Traindata3), Traindata3$V15)
testPred3 <- predict(boosting, newdata=Testdata3)
result = table(testPred3, Testdata3$V15);
cat("The Accuracy for",ratio,"Training is:")
accuracy5 = sum(sum(result * diag(2))) / sum(sum(result))
print(accuracy5)

bagging <- Bagging(V15~., data=Traindata3)
roc(as.factor(Testdata3$V15), predict(bagging, newdata = Testdata3, type = "probability")[,2])
table(predict(bagging, Traindata3), Traindata3$V15)
testPred3 <- predict(bagging, newdata=Testdata3)
result = table(testPred3, Testdata3$V15);
cat("The Accuracy for",ratio,"Training is:")
accuracy6 = sum(sum(result * diag(2))) / sum(sum(result))
print(accuracy6)
}