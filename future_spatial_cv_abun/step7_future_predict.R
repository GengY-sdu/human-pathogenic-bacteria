#step7:future predict###########################################################

rm(list = ls())
require(caret)
library(randomForest)
library(ggplot2)
require(pbapply)
require(doParallel)
require(foreach)

load(file = 'pathogen/future_spatial_cv_abun/history_dataset_nohuman_abun.Rdata')
Data <- history_dataset
colnames(Data)[1] <- "Response"
load(file = "pathogen/future_spatial_cv_abun/rffuncs_future_terra_nohuman_abun.rdata")
optvar <- rfFuncs$optVariables
max(rfFuncs$results$Rsquared)

set.seed(51)
load(file = 'pathogen/future_spatial_cv_abun/inTrain.Rdata')
data_train <- Data[inTrain,]
data_test <- Data[-inTrain,]
cols = c("Response",optvar)
data_train <- data_train[,..cols]
data_test <- data_test[,..cols]
trainx <- data_train[,-1]
trainy <- data_train$Response

load(file = "pathogen/future_spatial_cv_abun/custom_nohuman_abun.rdata")
fit2 <- custom

library(data.table)
current_data <-  dir('pathogen/clim_1970_2000/',full.names = TRUE)|>raster::stack()
current_data <- current_data|>terra::as.data.frame(xy =TRUE)
current_data <- current_data[complete.cases(current_data),]
colnames(current_data)[3:21] <- colnames(current_data)[3:21]|>stringr::str_extract("(?<=wc2.1_5m_).*")|>stringr::str_replace("_","")
current_data <- current_data[,c(1:3,14:21,4:13)]
colnames(current_data)[c(3:11)] <- paste0("bio0",1:9)
current_data$'current_pred' <- predict(fit2,newdata=current_data)

current_data <- current_data[,c(1:2,22)]
current_data$x <- round(current_data$x,8)
current_data$y <- round(current_data$y,8)
current_data$'id' <- paste0(current_data$x,'###',current_data$y)
current_data <- data.table(current_data)

dat_mess <- fread("pathogen/future_spatial_cv_abun/MESS_table_nohuman_abun.csv")
dat_mess$x <- round(dat_mess$x,8)
dat_mess$y <- round(dat_mess$y,8)
dat_mess$id <- paste0(dat_mess$x,'###',dat_mess$y)
dat_mess <- dat_mess[,c(4,3)]

current_data <- merge(current_data,dat_mess,by='id',all.x=T)
current_data <- current_data[which(current_data$mess>0),]
gc()

#step7.1:ssp126#################################################################

file_folder <- list.files("pathogen/clim_2081_2100/ssp126/",full.names = TRUE)
ssp126 <- list(1:length(file_folder))
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
  ssp126[[i]] <- map_pred[,c(3,4)]
  rm(map_point,map_pred);gc()
  print(Sys.time())
}
result <- ssp126[[1]]|>data.table()
for(i in 2:length(ssp126)){
  result <- dplyr::inner_join(result,ssp126[[i]],by = "id")
}
result$'average' <- apply(result[,-1],1,mean)

result <- merge(result,current_data,by='id')
result <- result[,c(1,17:20,2:16)]

result$'respone_chage' <- (result$average - result$current_pred)/result$current_pred
fwrite(result,"pathogen/future_spatial_cv_abun/ssp126_nohuman_abun.csv")
result <- result[,c(2,3,21)]

result <- fread(file = 'pathogen/future_spatial_cv_abun/ssp126_nohuman_abun.csv')
result <- result[,c(2,3,4,20,21)]
(sum(result$average)-sum(result$current_pred))/sum(result$current_pred)
dat2 <- result[,"lat_mean":=((sum(average)-sum(current_pred))/sum(current_pred)),by =c("y")]
dat2 <- unique(dat2,by = c("y"))
setorder(dat2, y)
write.csv(dat2,file = 'pathogen/future_spatial_cv_abun/ssp126_lat_abun.csv',row.names = F)

#step7.2:ssp245#################################################################

file_folder <- list.files("pathogen/clim_2081_2100/ssp245/",full.names = TRUE)
ssp245 <- list(1:length(file_folder))
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
  colnames(map_pred)[4] <- file_folder[[i]]|>stringr::str_extract("(?<=ssp245/).*(?=.tif)")
  ssp245[[i]] <- map_pred[,c(3,4)]
  rm(map_point,map_pred);gc()
  print(Sys.time())
}
result <- ssp245[[1]]|>data.table()
for(i in 2:length(ssp245)){
  result <- dplyr::inner_join(result,ssp245[[i]],by = "id")
}
result$'average' <- apply(result[,-1],1,mean)

result <- merge(result,current_data,by='id')
result <- result[,c(1,15:18,2:14)]

result$'respone_chage' <- (result$average - result$current_pred)/result$current_pred
fwrite(result,"pathogen/future_spatial_cv_abun/ssp245_nohuman_abun.csv")
result <- result[,c(2,3,19)]

result <- fread(file = 'pathogen/future_spatial_cv_abun/ssp245_nohuman_abun.csv')
result <- result[,c(2,3,4,18,19)]
(sum(result$average)-sum(result$current_pred))/sum(result$current_pred)
dat2 <- result[,"lat_mean":=((sum(average)-sum(current_pred))/sum(current_pred)),by =c("y")]
dat2 <- unique(dat2,by = c("y"))
setorder(dat2, y)
write.csv(dat2,file = 'pathogen/future_spatial_cv_abun/ssp245_lat_abun.csv',row.names = F)

#step7.3:ssp370#################################################################

file_folder <- list.files("pathogen/clim_2081_2100/ssp370/",full.names = TRUE)
ssp370 <- list(1:length(file_folder))
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
  colnames(map_pred)[4] <- file_folder[[i]]|>stringr::str_extract("(?<=370/).*(?=.tif)")
  ssp370[[i]] <- map_pred[,c(3,4)]
  rm(map_point,map_pred);gc()
  print(Sys.time())
}

result <- ssp370[[1]]|>data.table()
for(i in 2:length(ssp370)){
  result <- dplyr::inner_join(result,ssp370[[i]],by = "id")
}

result$'average' <- apply(result[,-1],1,mean)

result <- merge(result,current_data,by='id')
result <- result[,c(1,14:17,2:13)]

result$'respone_chage' <- (result$average - result$current_pred)/result$current_pred
fwrite(result,"pathogen/future_spatial_cv_abun/ssp370_nohuman_abun.csv")
result <- result[,c(2,3,18)]

result <- fread(file = 'pathogen/future_spatial_cv_abun/ssp370_nohuman_abun.csv')
result <- result[,c(2,3,4,17,18)]
(sum(result$average)-sum(result$current_pred))/sum(result$current_pred)
dat2 <- result[,"lat_mean":=((sum(average)-sum(current_pred))/sum(current_pred)),by =c("y")]
dat2 <- unique(dat2,by = c("y"))
setorder(dat2, y)
write.csv(dat2,file = 'pathogen/future_spatial_cv_abun/ssp370_lat_abun.csv',row.names = F)

#step7.4:ssp585#################################################################

file_folder <- list.files("pathogen/clim_2081_2100/ssp585/",full.names = TRUE)
ssp585 <- list(1:length(file_folder))
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
  colnames(map_pred)[4] <- file_folder[[i]]|>stringr::str_extract("(?<=585/).*(?=.tif)")
  ssp585[[i]] <- map_pred[,c(3,4)]
  rm(map_point,map_pred);gc()
  print(Sys.time())
}

result <- ssp585[[1]]|>data.table()
for(i in 2:length(ssp585)){
  result <- dplyr::inner_join(result,ssp585[[i]],by = "id")
}
result$'average' <- apply(result[,-1],1,mean)

result <- merge(result,current_data,by='id')
result <- result[,c(1,15:18,2:14)]

result$'respone_chage' <- (result$average - result$current_pred)/result$current_pred
fwrite(result,"pathogen/future_spatial_cv_abun/ssp585_nohuman_abun.csv")
result <- result[,c(2,3,19)]

result <- fread(file = 'pathogen/future_spatial_cv_abun/ssp585_nohuman_abun.csv')
result <- result[,c(2,3,4,18,19)]
(sum(result$average)-sum(result$current_pred))/sum(result$current_pred)
dat2 <- result[,"lat_mean":=((sum(average)-sum(current_pred))/sum(current_pred)),by =c("y")]
dat2 <- unique(dat2,by = c("y"))
setorder(dat2, y)
write.csv(dat2,file = 'pathogen/future_spatial_cv_abun/ssp585_lat_abun.csv',row.names = F)


#step7.5:final##################################################################

rm(list = ls())
ssp126 <- fread("pathogen/future_spatial_cv_abun/ssp126_nohuman_abun.csv")
ssp245 <- fread("pathogen/future_spatial_cv_abun/ssp245_nohuman_abun.csv")
ssp370 <- fread("pathogen/future_spatial_cv_abun/ssp370_nohuman_abun.csv")
ssp585 <- fread("pathogen/future_spatial_cv_abun/ssp585_nohuman_abun.csv")

total <- ssp126[,c(2,3,1,21)]
total <- total[ssp245[,c(1,19)],on = c("id")]
total <- total[ssp370[,c(1,18)],on = c("id")]
total <- total[ssp585[,c(1,19)],on = c("id")]
rm(ssp126,ssp245,ssp370,ssp585);gc()
colnames(total)[4:7] <- paste0("change_",c(126,245,370,585))

total$'level' <- apply(total[,c(4:7)],1,function(x){
  test = unlist(x)
  return(test[test>0]|>length())
})
write.csv(total,file = 'pathogen/future_spatial_cv_abun/future_final_abun.csv',row.names = F)

