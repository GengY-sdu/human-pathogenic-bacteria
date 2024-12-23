#step4:modelling################################################################
rm(list = ls())
require(caret)
library(randomForest)
library(ggplot2)
require(pbapply)
require(doParallel)
require(foreach)

load(file = 'pathogen/future_EMP/history_dataset_nohuman.Rdata')
Data <- history_dataset
colnames(Data)[1] <- "Response"
load(file = "pathogen/future_EMP/rffuncs_future_terra_nohuman.rdata")
optvar <- rfFuncs$optVariables
max(rfFuncs$results$Rsquared)

set.seed(44)
load(file = 'pathogen/future_EMP/inTrain.Rdata')
data_train <- Data[inTrain,]
data_test <- Data[-inTrain,]
cols = c("Response",optvar)
data_train <- data_train[,..cols]
data_test <- data_test[,..cols]
trainx <- data_train[,-1]
trainy <- data_train$Response

num_cores <- 10
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)
foreach(i = 1:num_cores) %dopar% {set.seed(44)}
control <- trainControl(method="cv", number=10,search='grid',savePredictions = 'final', allowParallel = T)
set.seed(44)
custom <- train(Response~., 
                data=data_train, method="rf", 
                metric = "Rsquared",importance=T,
                trControl=control)
stopCluster(cl)
save(custom,file = "pathogen/future_EMP/custom_nohuman.rdata")