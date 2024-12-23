#step4:hyperparameter tuning####################################################
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
load(file = 'pathogen/map_remove/optvar.Rdata')
load(file = 'pathogen/map_remove/ml_df.Rdata')
dat <- df_filter
dat2 <- na.omit(dat)
dat3 <- dat2[,-1]
x <- dat3[,optvar]  
logBBB <- dat2$mean_otu_diversity

load(file = 'pathogen/map_remove/inTrain.Rdata')
load(file = 'pathogen/map_remove/inTest.Rdata')
trainx <- x[inTrain, ]
testx <- x[inTest, ]
trainy <- logBBB[inTrain]
testy <- logBBB[inTest]

num_cores <- 10
cl <- makeCluster(num_cores) 
registerDoParallel(cl)
for (ntree in c(500,800,1000,1200,1500,2000,2500)) {
  Sys.time()
  foreach(i = 1:num_cores) %dopar% {set.seed(51)}
  control <- trainControl(method="cv", number=10,search='grid',savePredictions = 'final', allowParallel = T)
  tunegrid <- expand.grid(.mtry = c(1,3,5,8,10,12,15,18,20,22,25,30,35))
  set.seed(51)
  custom <- train(trainx,trainy,method="rf", 
                  metric = "Rsquared",importance=T,
                  tuneGrid=tunegrid,ntree = ntree,trControl=control)
  
  save(custom,file = paste0('pathogen/map_remove/HPB-',ntree,".Rdata"))
  Sys.time()
}
stopCluster(cl)