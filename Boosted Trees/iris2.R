library(glmnet)
library(gbm)
library(rpart)
library(plyr)
myData3 <-read.csv(file="/Users/ArvindhS/Classes/Machine Learning/Project/iris.csv", header=FALSE);
str(myData3)
library(caret)
set.seed(4909)
for (ratio in c(0.5,0.6,0.7,0.8,0.9)) {
train_ind<-createDataPartition(myData3$V5, p = ratio,
                                 list = FALSE,
                                 times = 1)
  Traindata3 <- myData3[ train_ind,]
  Testdata3  <- myData3[-train_ind,]
library(RWeka)
library(datasets)
fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)

gbmFit1 <- train(V5 ~ ., data = Traindata3, method = "gbm",trControl = fitControl, verbose=FALSE)

testPred3 <- predict(gbmFit1, newdata=Testdata3)
result = table(testPred3, Testdata3$V5);
cat("The Accuracy for",ratio,"Training is:")
accuracy4 = sum(sum(result * diag(3))) / sum(sum(result))
print(accuracy4)
}