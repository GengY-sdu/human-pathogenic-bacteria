#step4:feature selection########################################################
rm(list = ls())
library(tidymodels)
require(caret)
require(pbapply)
require(pbapply)
require(doParallel)
require(foreach)
library(blockCV)

set.seed(51)
load(file = 'pathogen/future_remove/history_dataset_nohuman.Rdata')
Data <- history_dataset
colnames(Data)[1] <- "Response"

set.seed(51)
load(file = 'pathogen/future_remove/location.Rdata')
points <- location
colnames(points) <- c('x','y','otu_diversity')
pa_data <- sf::st_as_sf(points, coords = c("x", "y"), crs = 4326)
pa_data_2 <- sf::st_transform(pa_data, "+proj=moll +ellps=WGS84 +datum=WGS84")
sb1 <- cv_spatial(x = pa_data_2,size =5000,k = 10,selection = "random",iteration = 50)

inTrain <- sb1$folds_list[[2]][[1]]
inTest <- sb1$folds_list[[2]][[2]]
save(inTrain,file = 'pathogen/future_remove/inTrain.Rdata')
save(inTest,file = 'pathogen/future_remove/inTest.Rdata')
data_train <- Data[inTrain,]
data_test <- Data[inTest,]
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
save(rfFuncs,file = "pathogen/future_remove/rffuncs_future_terra_nohuman.rdata")
