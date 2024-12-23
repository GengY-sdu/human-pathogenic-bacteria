#step3:feature selection########################################################
rm(list = ls())
library(caret)
library(ggplot2)
library(pbapply)
library(doParallel)
library(foreach)

set.seed(51)
load(file = 'pathogen/map_EMP/ml_df.Rdata')
dat <- df_filter
dat2 <- na.omit(dat)
dat3 <- dat2[,-1]
x <- dat3[,-nearZeroVar(dat3)]  
logBBB <- dat2$mean_otu_diversity

set.seed(51)
inTrain <- createDataPartition(logBBB, p = 0.9, list = FALSE)[,1]  
save(inTrain,file = 'pathogen/map_EMP/inTrain_nohuman.Rdata')
trainx <- x[inTrain, ]
testx <- x[-inTrain, ]
trainy <- logBBB[inTrain]
testy <- logBBB[-inTrain]

num_cores <- 10
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)
foreach(i = 1:num_cores) %dopar% {set.seed(51)}
control<-rfeControl(functions = rfFuncs,method = "cv",number = 10,rerank = T, allowParallel = T)
set.seed(51)
rfFuncs <- rfe(trainx, trainy,sizes = c(1:ncol(x)),rfeControl = control,metric = "Rsquared")
stopCluster(cl)
save(rfFuncs,file = 'pathogen/map_EMP/rfFuncs.Rdata')
variable <- rfFuncs$results$Variables
rmse <- rfFuncs$results$RMSE
mae <- rfFuncs$results$MAE
rsq <- rfFuncs$results$Rsquared
sum <- cbind(variable,rmse,mae,rsq)
optvar <- rfFuncs$optVariables
save(optvar,file = 'pathogen/map_EMP/optvar.Rdata')
write.csv(sum,file = 'pathogen/map_EMP/feature_set.csv',row.names = F)



