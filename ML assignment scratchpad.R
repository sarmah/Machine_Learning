library(caret)
library(gbm)
library(survival)
library(splines)
library(parallel)
library(plyr)
set.seed(8675309)

rm(list=ls())

## load data
training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", na.strings ="#DIV/0!")
testing  <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", na.strings ="#DIV/0!")

## set all columns to numeric except outcome
# for(i in 1:length(training[,-160])) {
#   training[,i] <- as.numeric(paste(training[,i]))
# }
# for(i in 1:length(testing[,-160])) {
#   testing[,i] <- as.numeric(paste(testing[,i]))
# }

## remove columns that are not measurements
training <- training[,-(1:5)]
testing  <- testing[,-(1:5)]

## identify and remove columns with NAs > 90%
x <- 0
for(i in 1:length(training)) {
  x[i] <- print(sum(is.na(training[,i]))/nrow(training))
}

training <- subset(training, select = x < 0.90)
testing  <- subset(testing, select = x < 0.90)

## remove columns with very low variances
noVar <- nearZeroVar(training)
training <- training[,-noVar]
testing  <- testing[,-noVar]

## Sub-sample the training data set into a sub training and testing data set
inTrain  <- createDataPartition(y=training$classe, p=0.70, list=FALSE)
subTrain <- training[inTrain,]
subTest  <- training[-inTrain,]

controlRF <- trainControl(method="cv", number=3, verboseIter=FALSE)
modRF  <- train(subTrain$classe~., method="rf", data=subTrain, trControl=controlRF)
outRF  <- predict(modRF, subTest)
conRF  <- confusionMatrix(subTest$classe, outRF)

modRP   <- train(subTrain$classe~., method="rpart", data=subTrain)
outRP   <- predict(modRP, subTest)
conRP  <- confusionMatrix(subTest$classe, outRP)

modGBM  <- train(subTrain$classe~., method="gbm", data=subTrain, verbose=FALSE)
outGBM  <- predict(modGBM, subTest)
conGBM <- confusionMatrix(subTest$classe, outGBM)

conRF$overall[1]
conRP$overall[1]
conGBM$overall[1]
conLDA$overall[1]

# pred <- predict(modGBM, testing)
# conPred <- confusionMatrix(testing$classe, pred)
# conPred$table