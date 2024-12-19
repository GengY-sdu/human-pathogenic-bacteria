#step5:modelling################################################################
rm(list = ls())
require(caret)
library(randomForest)
library(ggplot2)
require(pbapply)
require(doParallel)
require(foreach)

load(file = 'pathogen/future_spatial_cv/history_dataset_nohuman.Rdata')
Data <- history_dataset
colnames(Data)[1] <- "Response"
load("pathogen/future_spatial_cv/rffuncs_future_terra_nohuman.rdata")
optvar <- rfFuncs$optVariables
max(rfFuncs$results$Rsquared)

i <- 1
set.seed(i)
load(file = 'pathogen/future_spatial_cv/inTrain.Rdata')
data_train <- Data[inTrain,]
data_test <- Data[-inTrain,]
cols = c("Response",optvar)
data_train <- data_train[,..cols]
data_test <- data_test[,..cols]
trainx <- data_train[,-1]
trainy <- data_train$Response

load(file = 'pathogen/future_spatial_cv/sb1.Rdata')

num_cores <- 10
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)
foreach(i = 1:num_cores) %dopar% {set.seed(i)}
control <- trainControl(index = lapply(sb1$folds_list, function(x) x[[1]]),indexOut = lapply(sb1$folds_list, function(x) x[[2]]),search='grid',savePredictions = 'final', allowParallel = T)
set.seed(i)
custom <- train(Response~., 
                data=data_train, method="rf", 
                metric = "Rsquared",importance=T,
                trControl=control)
stopCluster(cl)
save(custom,file = "pathogen/future_spatial_cv/custom_nohuman.rdata")



