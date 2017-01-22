myData3 <-read.csv(file="/Users/ArvindhS/Classes/Machine Learning/Project/tictactoe.csv", header=FALSE, colClasses=c(rep('integer',27), 'factor'));
str(myData3)
library(caret)
set.seed(4909)
for (ratio in c(0.1, 0.2, 0.3, 0.4, 0.5)) {
train_ind<-createDataPartition(myData3$V28, p = ratio,
                         list = FALSE,
                         times = 1)
Traindata3 <- myData3[ train_ind,]
Testdata3  <- myData3[-train_ind,]
library(RWeka)
library(datasets)
library(e1071)
nb1<-naiveBayes(V28~., data=Traindata3) 
table(predict(nb1, Traindata3), Traindata3$V28)
testPred3 <- predict(nb1, newdata=Testdata3)
result = table(testPred3, Testdata3$V28);
cat("The Accuracy for",ratio,"Training is:")
accuracy4 = sum(sum(result * diag(2))) / sum(sum(result))
print(accuracy4)
}