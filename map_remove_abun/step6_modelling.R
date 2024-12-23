#step6:modelling################################################################
rm(list = ls())
library(caret)
library(randomForest)
library(ggplot2)
require(ggplot2)
require(caret)
require(randomForest)
require(future.apply)
require(parallel)
require(doParallel)
require(foreach)

set.seed(51)
load(file = 'pathogen/map_remove_abun/optvar.Rdata')
load(file = 'pathogen/map_remove_abun/ml_df_abun.Rdata')
dat <- df_filter
dat2 <- na.omit(dat)
dat3 <- dat2[,-1]
x <- dat3[,optvar]  
logBBB <- dat2$mean_otu_abun

load(file = 'pathogen/map_remove_abun/inTrain.Rdata')
load(file = 'pathogen/map_remove_abun/inTest.Rdata')
trainx <- x[inTrain, ]
testx <- x[inTest, ]
trainy <- logBBB[inTrain]
testy <- logBBB[inTest]

load("pathogen/map_remove_abun/HPB-1000_abun.Rdata")
rf_gridsearch <- custom

rsq <- function(x, y) summary(lm(y~x))$r.squared

rfmodel1 <- rf_gridsearch$finalModel
rfmodel1
predictRF_1 <- predict(rfmodel1,newdata = testx)
rsq(testy, predictRF_1)

set.seed(2)
cl <- makeCluster(10)
registerDoParallel(cl)
control_grid <- trainControl(method="cv", number=10,search='grid',savePredictions = 'final',allowParallel = T)
rfmodel2 <- train(trainx,trainy, method='rf',tuneGrid=rf_gridsearch$bestTune, 
                  ntree= 1000, trControl=control_grid, 
                  metric = "Rsquared",impotance=T)
stopCluster(cl)
rfmodel2
predictRF_2 <- predict(rfmodel2,newdata = testx)
rsq(testy, predictRF_2)

set.seed(3)
cl <- makeCluster(10)
registerDoParallel(cl)
control_grid <- trainControl(method="cv", number=10,search='grid',savePredictions = 'final',allowParallel = T)
rfmodel3 <- train(trainx,trainy, method='rf',tuneGrid=rf_gridsearch$bestTune, 
                  ntree= 1000, trControl=control_grid, 
                  metric = "Rsquared",impotance=T)
stopCluster(cl)
rfmodel3
predictRF_3 <- predict(rfmodel3,newdata = testx)
rsq(testy, predictRF_3)

set.seed(4)
cl <- makeCluster(10)
registerDoParallel(cl)
control_grid <- trainControl(method="cv", number=10,search='grid',savePredictions = 'final',allowParallel = T)
rfmodel4 <- train(trainx,trainy, method='rf',tuneGrid=rf_gridsearch$bestTune, 
                  ntree= 1000, trControl=control_grid, 
                  metric = "Rsquared",impotance=T)
stopCluster(cl)
rfmodel4
predictRF_4 <- predict(rfmodel4,newdata = testx)
rsq(testy, predictRF_4)

set.seed(5)
cl <- makeCluster(10)
registerDoParallel(cl)
control_grid <- trainControl(method="cv", number=10,search='grid',savePredictions = 'final',allowParallel = T)
rfmodel5 <- train(trainx,trainy, method='rf',tuneGrid=rf_gridsearch$bestTune, 
                  ntree= 1000, trControl=control_grid, 
                  metric = "Rsquared",impotance=T)
stopCluster(cl)
rfmodel5
predictRF_5 <- predict(rfmodel5,newdata = testx)
rsq(testy, predictRF_5)

set.seed(6)
cl <- makeCluster(10)
registerDoParallel(cl)
control_grid <- trainControl(method="cv", number=10,search='grid',savePredictions = 'final',allowParallel = T)
rfmodel6 <- train(trainx,trainy, method='rf',tuneGrid=rf_gridsearch$bestTune, 
                  ntree= 1000, trControl=control_grid, 
                  metric = "Rsquared",impotance=T)
stopCluster(cl)
rfmodel6
predictRF_6 <- predict(rfmodel6,newdata = testx)
rsq(testy, predictRF_6)

set.seed(7)
cl <- makeCluster(10)
registerDoParallel(cl)
control_grid <- trainControl(method="cv", number=10,search='grid',savePredictions = 'final',allowParallel = T)
rfmodel7 <- train(trainx,trainy, method='rf',tuneGrid=rf_gridsearch$bestTune, 
                  ntree= 1000, trControl=control_grid, 
                  metric = "Rsquared",impotance=T)
stopCluster(cl)
rfmodel7
predictRF_7 <- predict(rfmodel7,newdata = testx)
rsq(testy, predictRF_7)

set.seed(8)
cl <- makeCluster(10)
registerDoParallel(cl)
control_grid <- trainControl(method="cv", number=10,search='grid',savePredictions = 'final',allowParallel = T)
rfmodel8 <- train(trainx,trainy, method='rf',tuneGrid=rf_gridsearch$bestTune, 
                  ntree= 1000, trControl=control_grid, 
                  metric = "Rsquared",impotance=T)
stopCluster(cl)
rfmodel8
predictRF_8 <- predict(rfmodel8,newdata = testx)
rsq(testy, predictRF_8)

set.seed(9)
cl <- makeCluster(10)
registerDoParallel(cl)
control_grid <- trainControl(method="cv", number=10,search='grid',savePredictions = 'final',allowParallel = T)
rfmodel9 <- train(trainx,trainy, method='rf',tuneGrid=rf_gridsearch$bestTune, 
                  ntree= 1000, trControl=control_grid, 
                  metric = "Rsquared",impotance=T)
stopCluster(cl)
rfmodel9
predictRF_9 <- predict(rfmodel9,newdata = testx)
rsq(testy, predictRF_9)

set.seed(1)
cl <- makeCluster(10)
registerDoParallel(cl)
control_grid <- trainControl(method="cv", number=10,search='grid',savePredictions = 'final',allowParallel = T)
rfmodel10 <- train(trainx,trainy, method='rf',tuneGrid=rf_gridsearch$bestTune, 
                   ntree= 1000, trControl=control_grid, 
                   metric = "Rsquared",impotance=T)
stopCluster(cl)
rfmodel10
predictRF_10 <- predict(rfmodel10,newdata = testx)
rsq(testy, predictRF_10)

mapset <- read.csv(file = 'pathogen/map_remove_abun/remove_map_abun.csv')
predictRF1 <- predict(rfmodel1,newdata = mapset)
predictRF2 <- predict(rfmodel2,newdata = mapset)
predictRF3 <- predict(rfmodel3,newdata = mapset)
predictRF4 <- predict(rfmodel4,newdata = mapset)
predictRF5 <- predict(rfmodel5,newdata = mapset)
predictRF6 <- predict(rfmodel6,newdata = mapset)
predictRF7 <- predict(rfmodel7,newdata = mapset)
predictRF8 <- predict(rfmodel8,newdata = mapset)
predictRF9 <- predict(rfmodel9,newdata = mapset)
predictRF10 <- predict(rfmodel10,newdata = mapset)

predictsum <- data.frame(predictRF1,predictRF2,predictRF3,predictRF4,predictRF5,
                         predictRF6,predictRF7,predictRF8,predictRF9,predictRF10,
                         mapset$longitude,mapset$latitude)

for (i in 1:nrow(predictsum)){
  predictsum$prediction_sd[i] <- sd(as.numeric(predictsum[i,1:10]))
  predictsum$prediction_mean[i] <- mean(as.numeric(predictsum[i,1:10]))
}
predictsum$cv <- predictsum$prediction_sd / predictsum$prediction_mean
write.csv(predictsum,file = 'pathogen/map_remove_abun/map_remove_abun.csv')

var1 <- as.data.frame(importance(rfmodel1,type = 2))
var1$model1 <- var1$IncNodePurity / sum(var1$IncNodePurity) * 100

var2 <- as.data.frame(importance(rfmodel2$finalMode,type = 2))
var2$model2 <- var2$IncNodePurity / sum(var2$IncNodePurity) * 100

var3 <- as.data.frame(importance(rfmodel3$finalMode,type = 2))
var3$model3 <- var3$IncNodePurity / sum(var3$IncNodePurity) * 100

var4 <- as.data.frame(importance(rfmodel4$finalMode,type = 2))
var4$model4 <- var4$IncNodePurity / sum(var4$IncNodePurity) * 100

var5 <- as.data.frame(importance(rfmodel5$finalMode,type = 2))
var5$model5 <- var5$IncNodePurity / sum(var5$IncNodePurity) * 100

var6 <- as.data.frame(importance(rfmodel6$finalMode,type = 2))
var6$model6 <- var6$IncNodePurity / sum(var6$IncNodePurity) * 100

var7 <- as.data.frame(importance(rfmodel7$finalMode,type = 2))
var7$model7 <- var7$IncNodePurity / sum(var7$IncNodePurity) * 100

var8 <- as.data.frame(importance(rfmodel8$finalMode,type = 2))
var8$model8 <- var8$IncNodePurity / sum(var8$IncNodePurity) * 100

var9 <- as.data.frame(importance(rfmodel9$finalMode,type = 2))
var9$model9 <- var9$IncNodePurity / sum(var9$IncNodePurity) * 100

var10 <- as.data.frame(importance(rfmodel10$finalMode,type = 2))
var10$model10 <- var10$IncNodePurity / sum(var10$IncNodePurity) * 100

write.csv(var1,file = 'pathogen/map_remove_abun/importance_model1_set_abun.csv')
write.csv(var2,file = 'pathogen/map_remove_abun/importance_model2_set_abun.csv')
write.csv(var3,file = 'pathogen/map_remove_abun/importance_model3_set_abun.csv')
write.csv(var4,file = 'pathogen/map_remove_abun/importance_model4_set_abun.csv')
write.csv(var5,file = 'pathogen/map_remove_abun/importance_model5_set_abun.csv')
write.csv(var6,file = 'pathogen/map_remove_abun/importance_model6_set_abun.csv')
write.csv(var7,file = 'pathogen/map_remove_abun/importance_model7_set_abun.csv')
write.csv(var8,file = 'pathogen/map_remove_abun/importance_model8_set_abun.csv')
write.csv(var9,file = 'pathogen/map_remove_abun/importance_model9_set_abun.csv')
write.csv(var10,file = 'pathogen/map_remove_abun/importance_model10_set_abun.csv')


