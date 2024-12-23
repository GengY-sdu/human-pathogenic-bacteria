#step3:feature selection########################################################
rm(list = ls())
library(caret)
library(ggplot2)
library(pbapply)
library(doParallel)
library(foreach)
library(blockCV)

set.seed(51)
load(file = 'pathogen/map_remove/ml_df.Rdata')
dat <- df_filter
dat2 <- na.omit(dat)
dat3 <- dat2[,-1]
x <- dat3[,-nearZeroVar(dat3)]  
logBBB <- dat2$mean_otu_diversity

dat_sp <- cbind(logBBB,x)
points <- dat_sp[,c(2,3,1)]
colnames(points) <- c('x','y','otu_diversity')
pa_data <- sf::st_as_sf(points, coords = c("x", "y"), crs = 4326)
pa_data_2 <- sf::st_transform(pa_data, "+proj=moll +ellps=WGS84 +datum=WGS84")
sb1 <- cv_spatial(x = pa_data_2,size = 5000,k = 10,selection = "random",iteration = 50)

inTrain <- sb1$folds_list[[2]][[1]]
inTest <- sb1$folds_list[[2]][[2]]
save(inTrain,file = 'pathogen/map_remove/inTrain.Rdata')
save(inTest,file = 'pathogen/map_remove/inTest.Rdata')
trainx <- x[inTrain, ]
testx <- x[inTest, ]
trainy <- logBBB[inTrain]
testy <- logBBB[inTest]

num_cores <- 10
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)
foreach(i = 1:num_cores) %dopar% {set.seed(51)}
control<-rfeControl(functions = rfFuncs,method = "cv",number = 10,rerank = T, allowParallel = T)
set.seed(51)
rfFuncs <- rfe(trainx, trainy,sizes = c(1:ncol(x)),rfeControl = control,metric = "Rsquared")
stopCluster(cl)
save(rfFuncs,file = 'pathogen/map_remove/rfFuncs.Rdata')
variable <- rfFuncs$results$Variables
rmse <- rfFuncs$results$RMSE
mae <- rfFuncs$results$MAE
rsq <- rfFuncs$results$Rsquared
sum <- cbind(variable,rmse,mae,rsq)
optvar <- rfFuncs$optVariables
save(optvar,file = 'pathogen/map_remove/optvar.Rdata')
write.csv(sum,file = 'pathogen/map_remove/feature_set.csv',row.names = F)



