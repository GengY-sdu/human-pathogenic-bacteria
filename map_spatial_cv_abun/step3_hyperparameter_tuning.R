#step3:hyperparameter tuning####################################################
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
library(blockCV)

load(file = 'pathogen/map_spatial_cv_abun/optvar.Rdata')
load(file = 'pathogen/map_spatial_cv_abun/ml_df.Rdata')
dat <- df_filter
dat2 <- na.omit(dat)
dat3 <- dat2[,-1]
x <- dat3[,optvar]   
logBBB <- dat2$mean_otu_abun

i <- 1

set.seed(i)
load(file = 'pathogen/map_spatial_cv_abun/inTrain.Rdata')
trainx <- x[inTrain, ]
testx <- x[-inTrain, ]
trainy <- logBBB[inTrain]
testy <- logBBB[-inTrain]

load(file = 'pathogen/map_spatial_cv_abun/sb1.Rdata')

num_cores <- 10
cl <- makeCluster(num_cores) 
registerDoParallel(cl)
for (ntree in c(500,800,1000,1200,1500,2000,2500)) {
  Sys.time()
  foreach(i = 1:num_cores) %dopar% {set.seed(i)}
  control <- trainControl(index = lapply(sb1$folds_list, function(x) x[[1]]),indexOut = lapply(sb1$folds_list, function(x) x[[2]]),search='grid',savePredictions = 'final', allowParallel = T)
  tunegrid <- expand.grid(.mtry = c(1,2,4,6,8,10,12,14,16,18,20,22,24))
  set.seed(i)
  custom <- train(trainx,trainy,method="rf", 
                  metric = "Rsquared",importance=T,
                  tuneGrid=tunegrid,ntree = ntree,trControl=control)
  
  save(custom,file = paste0('pathogen/map_spatial_cv_abun/HPB-',ntree,"_abun.Rdata"))
  Sys.time()
}
stopCluster(cl)


