library(caret)
library(rpart)
library(rattle)	
library(randomForest)

training <- read.csv("data/pml-training.csv")
testing <- read.csv("data/pml-testing.csv")

testing <- testing[,-nearZeroVar(testing)]
sNa <- sapply(testing, function(x) mean(is.na(x))) > 0.95
testing <- testing[, IsNa == FALSE]
testing <- testing[, -(1:6)]

inTrain <- createDataPartition(training$classe, p= 0.7, list = FALSE)

TrainSet <- training[inTrain, ]
TestSet <- training[-inTrain,]

NZV <- nearZeroVar(TrainSet)
TrainSet <- TrainSet[,-NZV]
TestSet <- TestSet[,-NZV]

IsNa <- sapply(TrainSet, function(x) mean(is.na(x))) > 0.95
TrainSet <- TrainSet[, IsNa == FALSE]
TestSet  <- TestSet[, IsNa == FALSE]

TrainSet <- TrainSet[, -(1:6)]
TestSet  <- TestSet[, -(1:6)]

## Decision Tree

set.seed(1991)
FitTree <- rpart(classe ~ ., data = TrainSet, method = "class")
fancyRpartPlot(FitTree)
PredictTree <- predict(FitTree, TestSet, type = "class")
ConfMatTree <- confusionMatrix(PredictTree, TestSet$classe)
ConfMatTree

# Random Forests

set.seed(1991)
FitRandForest <- train(classe ~ ., data = TrainSet, method = "rf", number = 5, trControl = trainControl(method = "cv"))
PredictRandForest <- predict(FitRandForest, TestSet)
ConfMatRandForest <- confusionMatrix(PredictRandForest, TestSet$classe)
ConfMatRandForest

# Generalized Boosted Model  

set.seed(1991)
FitGBM <- train(classe ~ ., method = "gbm", data = TrainSet, verbose = FALSE)
PredictGBM <- predict(FitGBM, TestSet)
ConfMatGBM <- confusionMatrix(PredictGBM, TestSet$classe)
ConfMatGBM

# Prediction  

prediction <- predict(FitRandForest, testing)
prediction

