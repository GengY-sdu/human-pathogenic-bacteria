#step4:feature selection########################################################

rm(list = ls())
library(tidymodels)
require(caret)
require(pbapply)
require(pbapply)
require(doParallel)
require(foreach)
library(blockCV)

load(file = 'pathogen/future_spatial_cv/history_dataset_nohuman.Rdata')
Data <- history_dataset
colnames(Data)[1] <- "Response"

i <- 1
set.seed(i)
inTrain <- createDataPartition(Data$Response, p = 0.9, list = FALSE)[,1]
save(inTrain,file = 'pathogen/future_spatial_cv/inTrain.Rdata')
data_train <- Data[inTrain,]
data_test <- Data[-inTrain,]
trainx <- data_train[,-1]
trainy <- data_train$Response

set.seed(i)
load(file = 'pathogen/future_spatial_cv/location.Rdata')
points <- location[inTrain,]
colnames(points) <- c('x','y','otu_diversity')
pa_data <- sf::st_as_sf(points, coords = c("x", "y"), crs = 4326)
pa_data_2 <- sf::st_transform(pa_data, "+proj=moll +ellps=WGS84 +datum=WGS84")
sb1 <- cv_spatial(x = pa_data_2,size =5000,k = 10,selection = "random",iteration = 50)
save(sb1,file = 'pathogen/future_spatial_cv/sb1.Rdata')

num_cores <- 10
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)
foreach(i = 1:num_cores) %dopar% {set.seed(i)}
control <- rfeControl(functions = rfFuncs,index = lapply(sb1$folds_list, function(x) x[[1]]),indexOut = lapply(sb1$folds_list, function(x) x[[2]]),rerank = T,allowParallel = T)
set.seed(i)
rfFuncs <- rfe(trainx, trainy,sizes = c(1:19),rfeControl = control,metric = "Rsquared")
stopCluster(cl)
save(rfFuncs,file = "pathogen/future_spatial_cv/rffuncs_future_terra_nohuman.rdata")
