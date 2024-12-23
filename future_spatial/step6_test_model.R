#step6:test model###############################################################
library(caret)
rm(list = ls())

load(file = 'pathogen/future_spatial/history_dataset_nohuman.Rdata')
Data <- history_dataset
colnames(Data)[1] <- "Response"
load("pathogen/future_spatial/rffuncs_future_terra_nohuman.rdata")
optvar <- rfFuncs$optVariables
max(rfFuncs$results$Rsquared)

set.seed(51)
load(file = 'pathogen/future_spatial/inTrain.Rdata')
load(file = 'pathogen/future_spatial/inTest.Rdata')
data_train <- Data[inTrain,]
data_test <- Data[inTest,]
cols = c("Response",optvar)
data_train <- data_train[,..cols]
data_test <- data_test[,..cols]
trainx <- data_train[,-1]
trainy <- data_train$Response

DatVad <- data_test
load(file = 'pathogen/future_spatial/custom_nohuman.rdata')
plot_train <- data.frame(custom$pred$pred,custom$pred$obs)
write.csv(plot_train,file = 'pathogen/future_spatial/train_future_set.csv',row.names = F)
fit2 <- custom

DatVad$Predict <- predict(fit2,newdata = DatVad[,-1])
cor.test(DatVad$Response,DatVad$Predict)
yardstick::rmse(DatVad,truth=Response,estimate=Predict)
R2(DatVad$Response,DatVad$Predict)
plot_test <- data.frame(DatVad$Predict,DatVad$Response)
write.csv(plot_test,file = 'pathogen/future_spatial/test_future_set.csv',row.names = F)
