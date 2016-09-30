setwd("D:/BI/Coursera/Courses/10PracticalMachineLearning")
 
library(readr)
train <- read_csv("pml-training.csv") 
test <- read_csv("pml-testing.csv")

library(caret)
nzv1 <- nearZeroVar(train,names = TRUE,saveMetrics = TRUE)
names(nzv1)
head(nzv1)
# imp_var <- nzv1$nearZeroVar.train.
# 
# train2 <- train[,imp_var]


nzv2 <- subset(nzv1,nzv1$nzv=="FALSE" & nzv1$zeroVar=="FALSE")
dim(nzv2)
head(nzv2)
imp_var <- c(train$new_window)
# table(train$kurtosis_yaw_belt)
# table(train$skewness_yaw_belt)
# table(train$skewness_yaw_dumbbell)
# table(train$kurtosis_yaw_dumbbell)
# table(train$amplitude_yaw_dumbbell)

# create preprocess object
preProc <- preProcess(train[,c(-101,-160)],method=c("center", "scale","pca"))
# calculate PCs for training data
trainPC <- predict(preProc,train[,-160])
# run model on outcome and principle components
modelFit <- train(train$classe ~ .,method="glm",data=trainPC)
# calculate PCs for test data
testPC <- predict(preProc,test)
# compare results
confusionMatrix(test$problem_id,predict(modelFit,testPC))

#preObj <- preProcess(train[,c(-101,-160)], method=c("center", "scale","knnImpute"))

# # construct model
# modelFit <- train(train$classe ~ .,method="glm",preProcess=c("pca","knnImpute"),data=train)
# # print results of model
# confusionMatrix(testing$type,predict(modelFit,testing))

glm.result <- step(glm(classe~ .,family=binomial(logit),data = train))
summary(glm.result)
anova(glm.result, test="Chisq")

library(caret)
train[train == ""] <- NA
train <- test[complete.cases(train),]

test[test == ""] <- NA
test <- train[complete.cases(test),]

rf <- train(classe~.,data=train,method = "rf")
gbm <- train(classe~., data=train, method="gbm")

rf.result <- predict(rf, test)
gbm.result <- predict(gbm, vowel.test)

confusionMatrix(vowel.test$y, rf.result)$overall['Accuracy']
confusionMatrix(vowel.test$y,gbm.result)$overall['Accuracy']
