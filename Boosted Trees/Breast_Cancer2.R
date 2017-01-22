library(glmnet)
library(gbm)
library(rpart)
library(plyr)
myData2 <-read.csv(file="/Users/ArvindhS/Classes/Machine Learning/Project/cancer_data.csv", header=FALSE, colClasses=c(rep('integer', 9), 'factor'));
str(myData2)
library(caret)
set.seed(4909)
for (ratio in c(0.5,0.6,0.7,0.8,0.9)) {
smp_size <- floor(ratio * nrow(myData2))
train_ind <- sample(seq_len(nrow(myData2)), size = smp_size)
Traindata3 <- myData2[ train_ind,]
Testdata3  <- myData2[-train_ind,]
library(RWeka)
library(datasets)
fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)

gbmFit1 <- train(V10 ~ ., data = Traindata3, method = "gbm",trControl = fitControl, verbose=FALSE)

testPred3 <- predict(gbmFit1, newdata=Testdata3)
result = table(testPred3, Testdata3$V10);
cat("The Accuracy for",ratio,"Training is:")
accuracy4 = sum(sum(result * diag(9))) / sum(sum(result))
print(accuracy4)
}