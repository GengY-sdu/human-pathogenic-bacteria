#step6:test model###############################################################
rm(list = ls())
library(caret)

load(file = 'pathogen/future_remove_abun/history_dataset_nohuman_abun.Rdata')
Data <- history_dataset
colnames(Data)[1] <- "Response"
load("pathogen/future_remove_abun/rffuncs_future_terra_nohuman_abun.rdata")
optvar <- rfFuncs$optVariables
max(rfFuncs$results$Rsquared)

set.seed(51)
load(file = 'pathogen/future_remove_abun/inTrain.Rdata')
load(file = 'pathogen/future_remove_abun/inTest.Rdata')
data_train <- Data[inTrain,]
data_test <- Data[inTest,]
cols = c("Response",optvar)
data_train <- data_train[,..cols]
data_test <- data_test[,..cols]
trainx <- data_train[,-1]
trainy <- data_train$Response

DatVad <- data_test
load(file = 'pathogen/future_remove_abun/custom_nohuman_abun.rdata')
plot_train <- data.frame(custom$pred$pred,custom$pred$obs)
write.csv(plot_train,file = 'pathogen/future_remove_abun/train_future_set_abun.csv',row.names = F)
fit2 <- custom

DatVad$Predict <- predict(fit2,newdata = DatVad[,-1])
cor.test(DatVad$Response,DatVad$Predict)
yardstick::rmse(DatVad,truth=Response,estimate=Predict)
R2(DatVad$Response,DatVad$Predict)
plot_test <- data.frame(DatVad$Predict,DatVad$Response)
write.csv(plot_test,file = 'pathogen/future_remove_abun/test_future_set_abun.csv',row.names = F)
