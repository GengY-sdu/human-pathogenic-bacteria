#1##############################################################################

rm(list = ls())
require(raster)
require(data.table)
require(pbapply)
require(doParallel)
require(foreach)
cl <- makeCluster(10)
registerDoParallel(cl)
historial_clim <-  dir('/wc2.1_5m_bio/',full.names = TRUE)
total <- pblapply(historial_clim,function(x){
  df_0 <- data.table::fread("dat_future.csv",integer64 = "numeric")
  n1 <- raster::raster(x)
  BIO_centroid <- raster::extract(n1, df_0[,c(2,1)], method='simple', buffer=1000, fun=mean, df=TRUE)
  return(BIO_centroid)},cl = cl)
stopCluster(cl)
mapped_df <- dplyr::bind_cols(total)
mapped_df <- mapped_df[,c(seq(2,38,2))]
df0 <- data.table::fread("dat_future.csv",integer64 = "numeric")
mapped_df <- cbind(df0,mapped_df)
rm(df0,cl,total,num_cores)
df0_add <- data.table::fread("df0_add.csv",integer64 = "numeric")
mapped_df <- df0_add[mapped_df,on = c("lat_lon1")]
fwrite(mapped_df,"start_table.csv")
rm(df0_add,mapped_df)
rm(historial_clim)
gc()

#2.1############################################################################

rm(list = ls())
require(data.table)
require(raster)
require(dismo)
set.seed(1)
historial_clim <-  dir('/wc2.1_5m_bio/',full.names = TRUE)|>raster::stack()
history_dataset <- fread("start_table.csv",integer64 = "numeric")
history_dataset <- history_dataset[!history_dataset$otu_diversity %in% boxplot.stats(history_dataset$otu_diversity)$out,]
history_dataset <- history_dataset[,'mean_otu_diversity':=mean(otu_diversity),by = c("lat_lon1")]
history_dataset <- unique(history_dataset[,c(18,17,38,19:37)],by = c("y","x"))
history_dataset <- history_dataset[-which(history_dataset$mean_otu_diversity=='0'),]
history_dataset <- history_dataset[,-c(1:3)]
history_dataset <- history_dataset[complete.cases(history_dataset),]
suppressWarnings(ms_report <- mess(historial_clim, history_dataset, full=TRUE))
plot(ms_report$mess)
result <- ms_report$mess|>as.data.frame(xy =TRUE)
result <- result[complete.cases(result),]
fwrite(result,"MESS_table.csv")

#2.2############################################################################

rm(list = ls())
require(data.table)
dat_mess <- fread("MESS_table.csv")
require(RColorBrewer)
require(ggplot2)
p <- ggplot(data=dat_mess,aes(x = x, y = y, fill = mess))+
  geom_tile()+
  scale_fill_fermenter(palette = "RdYlBu",direction = 1)+
  labs(fill = 'MESS value')+
  scale_x_continuous(breaks = seq(-180,180,90),expand = c(0,10))+
  scale_y_continuous(breaks = seq(-90,90,90),expand = c(0,10))+
  theme_classic()+
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor =  element_blank(),
        legend.position = "top" ,legend.box = "horizontal",
        legend.title = element_text(
          hjust = 0.5,
          colour = "black"),
        legend.title.position = "top"
  )+
  coord_fixed()
p
ggsave(p,filename = "mess.png",width = 10.4, height = 5.62, dpi = 300)

#3##############################################################################

rm(list = ls())
require(data.table)
history_dataset <- fread("start_table.csv",integer64 = "numeric")
history_dataset <- history_dataset[!history_dataset$otu_diversity %in% boxplot.stats(history_dataset$otu_diversity)$out,]
history_dataset <- history_dataset[,'mean_otu_diversity':=mean(otu_diversity),by = c("lat_lon1")]
history_dataset <- unique(history_dataset[,c(18,17,38,19:37)],by = c("y","x"))
history_dataset <- history_dataset[-which(history_dataset$mean_otu_diversity=='0'),]
history_dataset <- history_dataset[,-c(1:2)]
history_dataset <- history_dataset[complete.cases(history_dataset),]
colnames(history_dataset)[2:20] <- colnames(history_dataset)[2:20]|>stringr::str_extract("(?<=wc2.1_5m_).*")
colnames(history_dataset)[2:20] <- colnames(history_dataset)[2:20]|>stringr::str_replace("_","")
history_dataset <- history_dataset[,c(1,2,13:20,3:12)]
colnames(history_dataset)[2:10] <- paste0("bio0",1:9)
colnames(history_dataset)
save(history_dataset,file = 'history_dataset.Rdata')

#4##############################################################################

rm(list = ls())
library(tidymodels)
require(caret)
require(pbapply)
require(pbapply)
require(doParallel)
require(foreach)

load(file = 'history_dataset.Rdata')
Data <- history_dataset
colnames(Data)[1] <- "Response"

set.seed(51)
inTrain <- createDataPartition(Data$Response, p = 2/3, list = FALSE)[,1]  
data_train <- Data[inTrain,]
data_test <- Data[-inTrain,]
trainx <- data_train[,-1]
trainy <- data_train$Response

num_cores <- 10
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)
set.seed(51)

control<-rfeControl(functions = rfFuncs,method = "cv",number = 10, rerank = T, allowParallel=TRUE)
rfFuncs <- rfe(trainx, trainy,sizes = c(1:19),rfeControl = control,metric = "Rsquared")
stopCluster(cl)
save(rfFuncs,file = "rffuncs_future_terra.rdata")

#5##############################################################################

rm(list = ls())
require(caret)
library(randomForest)
library(ggplot2)
require(pbapply)
require(doParallel)
require(foreach)

load(file = 'history_dataset.Rdata')
Data <- history_dataset
colnames(Data)[1] <- "Response"
load("rffuncs_future_terra.rdata")
optvar <- rfFuncs$optVariables
max(rfFuncs$results$Rsquared)

set.seed(51)
inTrain <- createDataPartition(Data$Response, p = 2/3, list = FALSE)[,1]   
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
set.seed(51)
control <- trainControl(method="cv", number=10, search = 'grid',savePredictions = 'final',allowParallel = T)
custom <- train(Response~., 
                data=data_train, method="rf", 
                metric = "Rsquared",importance=T,
                trControl=control)
stopCluster(cl)
save(custom,file = "custom.rdata")

library(scales)
library(ggpmisc)
DatTrain <- data.frame(custom$pred$obs,custom$pred$pred)
colnames(DatTrain) <- c("Response","Predict")
p <- ggplot(DatTrain,aes(Response,Predict))+
  geom_point(aes(Response,Predict),color="grey80",size=2)+
  geom_smooth(aes(Response,Predict),method = "lm",
              fill="lightblue",color="black",linewidth=0.8)+
  stat_correlation(mapping = use_label(c("R","P","n","method")),
                   small.r = T,small.p = T,size=4,r.digits = 3)+
  labs(x="Observed relative richness",
       y="Predicted relative richness",
       title = "Cross-validation on the abundance-climate model")+
  theme_bw(base_size = 14)
p
ggsave(p,filename = "check-train.png",width = 5.62, height = 5.62, dpi = 300)

#6##############################################################################

DatVad <- data_test
fit2 <- custom

DatVad$Predict <- predict(fit2,newdata = DatVad[,-1])
cor.test(DatVad$Response,DatVad$Predict)
yardstick::rmse(DatVad,truth=Response,estimate=Predict)

library(scales)
library(ggpmisc)
p <- ggplot(DatVad,aes(Response,Predict))+
  geom_point(aes(Response,Predict),color="grey80",size=2)+
  geom_smooth(aes(Response,Predict),method = "lm",
              fill="lightblue",color="black",linewidth=0.8)+
  stat_correlation(mapping = use_label(c("R","P","n","method")),
                   small.r = T,small.p = T,size=4,r.digits = 3)+
  labs(x="Observed relative richness",
       y="Predicted relative richness",
       title = "Cross-validation on the abundance-climate model")+
 theme_bw(base_size = 14)
p
ggsave(p,filename = "check-test.png",width = 5.62, height = 5.62, dpi = 300)

#7##############################################################################

rm(list = ls())
require(caret)
library(randomForest)
library(ggplot2)
require(pbapply)
require(doParallel)
require(foreach)

load(file = 'history_dataset.Rdata')
Data <- history_dataset
colnames(Data)[1] <- "Response"
load(file = "rffuncs_future_terra.rdata")
optvar <- rfFuncs$optVariables
max(rfFuncs$results$Rsquared)

set.seed(51)
inTrain <- createDataPartition(Data$Response, p = 2/3, list = FALSE)[,1]   
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
current_data <-  dir('/wc2.1_5m_bio/',full.names = TRUE)|>raster::stack()
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

dat_mess <- fread("MESS_table.csv")
dat_mess$x <- round(dat_mess$x,8)
dat_mess$y <- round(dat_mess$y,8)
dat_mess$id <- paste0(dat_mess$x,'###',dat_mess$y)
dat_mess <- dat_mess[,c(4,3)]

current_data <- merge(current_data,dat_mess,by='id',all.x=T)
current_data <- current_data[which(current_data$mess>0),]
gc()

#8.1.1##########################################################################

file_folder <- list.files("/clim_2081_2100/sspxxx/",full.names = TRUE)
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
  colnames(map_pred)[4] <- file_folder[[i]]|>stringr::str_extract("(?<=sspxxx/).*(?=.tif)")
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
result <- result[,c(1,17:20,2:16)]

result$'respone_chage' <- (result$average - result$current_pred)/result$current_pred
fwrite(result,"sspxxx.csv")
result <- result[,c(2,3,21)]

#8.1.2##########################################################################

p <- ggplot(data=result,aes(x = x, y = y, fill = respone_chage))+
  geom_tile()+
  scale_fill_fermenter(palette = "RdBu",direction = -1,breaks =c(-1,-0.5,0,0.5))+
  scale_x_continuous(breaks = seq(-180,180,90),expand = c(0,10))+
  scale_y_continuous(breaks = seq(-90,90,90),expand = c(0,10))+
  theme_classic()+
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor =  element_blank(),
        legend.position = "top" ,legend.box = "horizontal",
        legend.title = element_text(
          hjust = 0.5,
          colour = "black"),
        legend.title.position = "top")+
  coord_fixed()
p
ggsave(p,filename = "sspxxx-1.png",width = 10.4, height = 5.62, dpi = 300)

result <- fread(file = 'sspxxx.csv')
result <- result[,c(2,3,4,20,21)]
(sum(result$average)-sum(result$current_pred))/sum(result$current_pred)
dat2 <- result[,"lat_mean":=((sum(average)-sum(current_pred))/sum(current_pred)),by =c("y")]
dat2 <- unique(dat2,by = c("y"))
setorder(dat2, y)
write.csv(dat2,file = 'sspxxx_lat.csv',row.names = F)

p_r <- ggplot(dat2,aes(y = lat_mean,x = y))+
  geom_line(color = "#2266ac")+
  geom_hline(yintercept =(sum(result$average)-sum(result$current_pred))/sum(result$current_pred),color = "red")+
  annotate("text", x = -80 , y = (sum(result$average)-sum(result$current_pred))/sum(result$current_pred)+0.02,
           label = paste0("Mean: \n",(sum(result$average)-sum(result$current_pred))/sum(result$current_pred)*100,"%"),colour="red")+
  scale_y_continuous(labels = label_percent())+
  coord_flip()+
  theme_classic()+
  labs(x="Latitude",
       y="Relative change",
       title = "sspxxx")
p_r
ggsave(p_r,filename = "sspxxx-2.png",width = 5.62, height = 5.62, dpi = 300)

rm(sspxxx,result,dat2);gc()



#9##############################################################################

rm(list = ls())
ssp126 <- fread("ssp126.csv")
ssp245 <- fread("ssp245.csv")
ssp370 <- fread("ssp370.csv")
ssp585 <- fread("ssp585.csv")

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

p <- ggplot(data=total,aes(x = x, y = y, fill = factor(level)))+
  geom_tile()+
  scale_fill_brewer(palette = "RdBu",direction = -1)+
  scale_x_continuous(breaks = seq(-180,180,90),expand = c(0,10))+
  scale_y_continuous(breaks = seq(-90,90,90),expand = c(0,10))+
  theme_classic()+
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor =  element_blank(),
        legend.position = "top" ,legend.box = "horizontal",
        legend.title = element_text(
          hjust = 0.5,
          colour = "black"),
        legend.title.position = "top"
  )+
  coord_fixed()
p
ggsave(p,filename = "final.png",width = 10.4, height = 5.62, dpi = 300)


