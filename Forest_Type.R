myData1 <-read.csv(file="/Users/ArvindhS/Classes/Machine Learning/Project/training.csv", header=TRUE, sep=",")
str(myData1)
myData2 <-read.csv(file="/Users/ArvindhS/Classes/Machine Learning/Project/testing.csv", header=TRUE, sep=",")
str(myData2)
library(caret)
set.seed(4909)
for (ratio in c(0.1, 0.2, 0.3, 0.4, 0.5)) {
ind<-createDataPartition(myData1$class, p = ratio,list = FALSE,times = 1)
Traindata3 <- myData1[ ind,]
ind1<-createDataPartition(myData1$class, p = 1-ratio,list = FALSE,times = 1)
Testdata3  <- myData2[ ind1,]
library(RWeka)
library(datasets)
library(e1071)
nb1<-naiveBayes(class~., data=Traindata3) 
table(predict(nb1, Traindata3), Traindata3$class)
testPred3 <- predict(nb1, newdata=Testdata3)
result = table(testPred3, Testdata3$class);
cat("The Accuracy for",ratio,"Training is:")
accuracy = sum(sum(result * diag(4))) / sum(sum(result))
print(accuracy)
}