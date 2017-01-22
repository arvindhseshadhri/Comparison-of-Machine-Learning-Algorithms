myData2 <-read.csv(file="/Users/ArvindhS/Classes/Machine Learning/Project/cancer_data.csv", header=FALSE, colClasses=c(rep('integer', 9), 'factor'));
str(myData2)
library(caret)
set.seed(4909)
for (ratio in c(0.1, 0.2, 0.3, 0.4, 0.5)) {
smp_size <- floor(ratio * nrow(myData2))
train_ind <- sample(seq_len(nrow(myData2)), size = smp_size)
Traindata3 <- myData2[ train_ind,]
Testdata3  <- myData2[-train_ind,]
library(RWeka)
library(datasets)
library(pROC)
stump <- DecisionStump(V10~., data=Traindata3)
print(stump)
table(predict(stump, Traindata3), Traindata3$V10)
testPred3 <- predict(stump, newdata=Testdata3)
result = table(testPred3, Testdata3$V10);
cat("The Accuracy for",ratio,"Training is:")
accuracy4 = sum(sum(result * diag(9))) / sum(sum(result))
print(accuracy4)

boosting <- AdaBoostM1(V10~., data=Traindata3)
table(predict(boosting, Traindata3), Traindata3$V10)
testPred3 <- predict(boosting, newdata=Testdata3)
result = table(testPred3, Testdata3$V10);
cat("The Accuracy for",ratio,"Training is:")
accuracy5 = sum(sum(result * diag(9))) / sum(sum(result))
print(accuracy5)

bagging <- Bagging(V10~., data=Traindata3)
table(predict(bagging, Traindata3), Traindata3$V10)
testPred3 <- predict(bagging, newdata=Testdata3)
result = table(testPred3, Testdata3$V10);
cat("The Accuracy for",ratio,"Training is:")
accuracy6 = sum(sum(result * diag(9))) / sum(sum(result))
print(accuracy6)
}