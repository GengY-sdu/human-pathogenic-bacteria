#step4:feature selection###############################################################
rm(list = ls())
library(tidymodels)
require(caret)
require(pbapply)
require(pbapply)
require(doParallel)
require(foreach)

set.seed(51)
load(file = 'pathogen/future_EMP/history_dataset_nohuman.Rdata')
Data <- history_dataset
colnames(Data)[1] <- "Response"

set.seed(51)
inTrain <- createDataPartition(Data$Response, p = 0.9, list = FALSE)[,1]
save(inTrain,file = 'pathogen/future_EMP/inTrain.Rdata')
data_train <- Data[inTrain,]
data_test <- Data[-inTrain,]
trainx <- data_train[,-1]
trainy <- data_train$Response

num_cores <- 10
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)
foreach(i = 1:num_cores) %dopar% {set.seed(51)}
control<-rfeControl(functions = rfFuncs,method = "cv",number = 10,rerank = T, allowParallel = T)
set.seed(51)
rfFuncs <- rfe(trainx, trainy,sizes = c(1:19),rfeControl = control,metric = "Rsquared")
stopCluster(cl)
save(rfFuncs,file = "pathogen/future_EMP/rffuncs_future_terra_nohuman.rdata")
variable <- rfFuncs$results$Variables
rmse <- rfFuncs$results$RMSE
mae <- rfFuncs$results$MAE
rsq <- rfFuncs$results$Rsquared
sum <- cbind(variable,rmse,mae,rsq)