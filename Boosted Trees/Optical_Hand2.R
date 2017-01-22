library(glmnet)
library(gbm)
library(rpart)
library(plyr)
myData3 <-read.csv(file="/Users/ArvindhS/Classes/Machine Learning/Project/optdigits_tra.csv", header=FALSE, colClasses=c(rep('integer', 64), 'factor'));
str(myData3)
myData4 <-read.csv(file="/Users/ArvindhS/Classes/Machine Learning/Project/optdigits_tes.csv", header=FALSE, colClasses=c(rep('integer', 64), 'factor'));
str(myData4)
library(caret)
set.seed(4909)
for (ratio in c(0.1, 0.2, 0.3, 0.4, 0.5)) {
smp_size <- floor(ratio * nrow(myData3))
train_ind <- sample(seq_len(nrow(myData3)), size = smp_size)
Traindata3 <- myData3[ train_ind,]
smp_size1 <- floor((1-ratio) * nrow(myData4))
train_ind1 <- sample(seq_len(nrow(myData4)), size = smp_size1)
Testdata3  <- myData4[ train_ind1,]
library(RWeka)
library(datasets)
fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)

gbmFit1 <- train(V65 ~ ., data = Traindata3, method = "gbm",trControl = fitControl, verbose=FALSE)

testPred3 <- predict(gbmFit1, newdata=Testdata3)
result = table(testPred3, Testdata3$V65);
cat("The Accuracy for",ratio,"Training is:")
accuracy4 = sum(sum(result * diag(10))) / sum(sum(result))
print(accuracy4)
}