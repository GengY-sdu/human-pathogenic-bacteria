################################################################################

rm(list = ls())
require(data.table)
require(raster)
require(dismo)

historial_clim <-  dir('/current/',full.names = TRUE)|>raster::stack()
history_dataset <- fread("dat.csv",integer64 = "numeric")

suppressWarnings(ms_report <- mess(historial_clim, history_dataset, full=TRUE))

result <- ms_report$mess|>as.data.frame(xy =TRUE)
result <- result[complete.cases(result),]
fwrite(result,"MESS_table.csv")
save(history_dataset,file = 'dat.Rdata')

################################################################################

rm(list = ls())
library(tidymodels)
require(caret)
require(pbapply)
require(pbapply)
require(doParallel)
require(foreach)

load(file = 'dat.Rdata')
Data <- history_dataset

inTrain <- createDataPartition(Data$Response, p = 2/3, list = FALSE)[,1]  
data_train <- Data[inTrain,]
data_test <- Data[-inTrain,]
trainx <- data_train[,-1]
trainy <- data_train$Response

num_cores <- 10
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)

control<-rfeControl(functions = rfFuncs,method = "cv",number = 10, rerank = T, allowParallel=TRUE)
rfFuncs <- rfe(trainx, trainy,sizes = c(1:19),rfeControl = control,metric = "Rsquared")

stopCluster(cl)
save(rfFuncs,file = "rffuncs.rdata")

################################################################################

rm(list = ls())
require(caret)
library(randomForest)
library(ggplot2)
require(pbapply)
require(doParallel)
require(foreach)

load(file = 'dat.Rdata')
Data <- history_dataset
colnames(Data)[1] <- "Response"
load("rffuncs.rdata")
optvar <- rfFuncs$optVariables

inTrain <- createDataPartition(Data$Response, p = 2/3, list = FALSE)[,1]   #将数据集分为训练集和测试集
data_train <- Data[inTrain,]
data_test <- Data[-inTrain,]
cols = c("Response",optvar)
data_train <- data_train[,..cols]
data_test <- data_test[,..cols]
trainx <- data_train[,-1]
trainy <- data_train$Response

num_cores <- 10
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)

control <- trainControl(method="cv", number=10, search = 'grid',savePredictions = 'final',allowParallel = T)
custom <- train(Response~., 
                data=data_train, method="rf", 
                metric = "Rsquared",importance=T,
                trControl=control)
stopCluster(cl)
save(custom,file = "custom.rdata")

DatVad <- data_test
fit2 <- custom
DatVad$Predict <- predict(fit2,newdata = DatVad[,-1])

################################################################################

rm(list = ls())
require(caret)
library(randomForest)
library(ggplot2)
require(pbapply)
require(doParallel)
require(foreach)

load(file = 'dat.Rdata')
Data <- history_dataset
colnames(Data)[1] <- "Response"
load(file = "rffuncs.rdata")
optvar <- rfFuncs$optVariables

inTrain <- createDataPartition(Data$Response, p = 2/3, list = FALSE)[,1]   #将数据集分为训练集和测试集
data_train <- Data[inTrain,]
data_test <- Data[-inTrain,]
cols = c("Response",optvar)
data_train <- data_train[,..cols]
data_test <- data_test[,..cols]
trainx <- data_train[,-1]
trainy <- data_train$Response

load(file = "custom.rdata")
fit2 <- custom

library(data.table)
current_data <-  dir('/current/',full.names = TRUE)|>raster::stack()
current_data <- current_data|>terra::as.data.frame(xy =TRUE)

current_data$'current_pred' <- predict(fit2,newdata=current_data)

file_folder <- list.files("/future/sspxxx/",full.names = TRUE)
sspxxx <- list(1:length(file_folder))
for(i in 1:length(file_folder)){
  print(i)
  print(Sys.time())
  map_point = terra::rast(file_folder[[i]])|>raster::stack()
  map_point = map_point|>terra::as.data.frame(xy = TRUE)
  map_point = map_point[complete.cases(map_point),]
  nrow(map_point)|>print()
  map_pred <- map_point[,c(1:2)]
  map_pred$x <- round(map_pred$x,8)
  map_pred$y <- round(map_pred$y,8)
  map_pred$'id' <- paste0(map_pred$x,"###",map_pred$y)
  map_pred$'pred' <- predict(fit2,newdata=map_point)
  colnames(map_pred)[4] <- file_folder[[i]]|>stringr::str_extract("(?<=ssp126/).*(?=.tif)")
  sspxxx[[i]] <- map_pred[,c(3,4)]
  rm(map_point,map_pred);gc()
  print(Sys.time())
}
result <- sspxxx[[1]]|>data.table()
for(i in 2:length(sspxxx)){
  result <- dplyr::inner_join(result,sspxxx[[i]],by = "id")
}
result$'average' <- apply(result[,-1],1,mean)

result <- merge(result,current_data,by='id')

result$'respone_chage' <- (result$average - result$current_pred)/result$current_pred
fwrite(result,"sspxxx.csv")

################################################################################

rm(list = ls())
ssp126 <- fread("ssp126.csv")
ssp245 <- fread("ssp245.csv")
ssp370 <- fread("ssp370.csv")
ssp585 <- fread("ssp585.csv")

total <- ssp126
total <- total[ssp245,on = c("id")]
total <- total[ssp370,on = c("id")]
total <- total[ssp585,on = c("id")]

total$'level' <- apply(total,1,function(x){
  test = unlist(x)
  return(test[test>0]|>length())
})



