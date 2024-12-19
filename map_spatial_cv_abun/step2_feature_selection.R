#step2:feature selection########################################################
rm(list = ls())
library(caret)
library(ggplot2)
library(pbapply)
library(doParallel)
library(foreach)
library(blockCV)

load(file = 'pathogen/map_spatial_cv_abun/ml_df.Rdata')
dat <- df_filter
dat2 <- na.omit(dat)
dat3 <- dat2[,-1]
x <- dat3[,-nearZeroVar(dat3)]   
logBBB <- dat2$mean_otu_abun

i <- 1

set.seed(i)
inTrain <- createDataPartition(logBBB, p = 0.9, list = FALSE)[,1]
save(inTrain,file = 'pathogen/map_spatial_cv_abun/inTrain.Rdata')
trainx <- x[inTrain, ]
testx <- x[-inTrain, ]
trainy <- logBBB[inTrain]
testy <- logBBB[-inTrain]

set.seed(i)
dat_sp <- cbind(trainy,trainx)
points <- dat_sp[,c(2,3,1)]
colnames(points) <- c('x','y','otu_abun')
pa_data <- sf::st_as_sf(points, coords = c("x", "y"), crs = 4326)
pa_data_2 <- sf::st_transform(pa_data, "+proj=moll +ellps=WGS84 +datum=WGS84")
sb1 <- cv_spatial(x = pa_data_2,size =5000,k = 10,selection = "random",iteration = 50)
save(sb1,file = 'pathogen/map_spatial_cv_abun/sb1.Rdata')

num_cores <- 10
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)
foreach(i = 1:num_cores) %dopar% {set.seed(i)}
control <- rfeControl(functions = rfFuncs,index = lapply(sb1$folds_list, function(x) x[[1]]),indexOut = lapply(sb1$folds_list, function(x) x[[2]]),rerank = T,allowParallel = T)
set.seed(i)
rfFuncs <- rfe(trainx, trainy,sizes = seq(1:ncol(x)), rfeControl = control,metric = "Rsquared")
stopCluster(cl)
save(rfFuncs,file = 'pathogen/map_spatial_cv_abun/rfFuncs.Rdata')
variable <- rfFuncs$results$Variables
rmse <- rfFuncs$results$RMSE
mae <- rfFuncs$results$MAE
rsq <- rfFuncs$results$Rsquared
sum <- cbind(variable,rmse,mae,rsq)
optvar <- rfFuncs$optVariables
save(optvar,file = 'pathogen/map_spatial_cv_abun/optvar.Rdata')
write.csv(sum,file = 'pathogen/map_spatial_cv_abun/feature_cv_abun.csv',row.names = F)



