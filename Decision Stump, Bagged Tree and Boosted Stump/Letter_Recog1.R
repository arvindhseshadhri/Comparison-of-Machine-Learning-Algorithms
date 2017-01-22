myData3 <-read.csv(file="/Users/ArvindhS/Classes/Machine Learning/Project/letter_recog.csv", header=FALSE, colClasses=c('factor', rep('integer',16)));
str(myData3)
library(caret)
set.seed(4909)
for (ratio in c(0.1, 0.2, 0.3, 0.4, 0.5)) {
train_ind<-createDataPartition(myData3$V1, p = ratio,
                               list = FALSE,
                               times = 1)
Traindata3 <- myData3[ train_ind,]
Testdata3  <- myData3[-train_ind,]
library(RWeka)
library(datasets)
library(pROC)
stump <- DecisionStump(V1~., data=Traindata3)
print(stump)
table(predict(stump, Traindata3), Traindata3$V1)
testPred3 <- predict(stump, newdata=Testdata3)
result = table(testPred3, Testdata3$V1);
cat("The Accuracy for",ratio,"Training is:")
accuracy4 = sum(sum(result * diag(26))) / sum(sum(result))
print(accuracy4)

bagging <- Bagging(V1~., data=Traindata3)
table(predict(bagging, Traindata3), Traindata3$V1)
testPred3 <- predict(bagging, newdata=Testdata3)
result = table(testPred3, Testdata3$V1);
cat("The Accuracy for",ratio,"Training is:")
accuracy6 = sum(sum(result * diag(26))) / sum(sum(result))
print(accuracy6)

boosting <- AdaBoostM1(V1~., data=Traindata3)
table(predict(boosting, Traindata3), Traindata3$V1)
testPred3 <- predict(boosting, newdata=Testdata3)
result = table(testPred3, Testdata3$V1);
cat("The Accuracy for",ratio,"Training is:")
accuracy5 = sum(sum(result * diag(26))) / sum(sum(result))
print(accuracy5)
}