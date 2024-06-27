#step1##########################################################################

rm(list = ls())
library(caret)
library(ggplot2)
library(pbapply)
library(doParallel)
library(foreach)

set.seed(51)
dat <- read.csv(file = 'dat.csv',header = T)
dat2 <- na.omit(dat)
dat3 <- dat2[,-1]
x <- dat3[,-nearZeroVar(dat3)]   
logBBB <- dat2$mean_otu_diversity
inTrain <- createDataPartition(logBBB, p = 0.8, list = FALSE)[,1]  
save(inTrain,file = 'inTrain.Rdata')
trainx <- x[inTrain, ]
testx <- x[-inTrain, ]
trainy <- logBBB[inTrain]
testy <- logBBB[-inTrain]

num_cores <- 15
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)
set.seed(51)
control<-rfeControl(functions = rfFuncs,method = "cv",number = 10,rerank = T, allowParallel = T)  
rfFuncs <- rfe(trainx, trainy,sizes = c(1:86),rfeControl = control,metric = "Rsquared")
stopCluster(cl)
save(rfFuncs,file = 'rfFuncs.Rdata')
variable <- rfFuncs$results$Variables
rmse <- rfFuncs$results$RMSE
mae <- rfFuncs$results$MAE
rsq <- rfFuncs$results$Rsquared
sum <- cbind(variable,rmse,mae,rsq)
optvar <- rfFuncs$optVariables
save(optvar,file = 'optvar51.Rdata')

plot <- data.frame(rfFuncs$results$Rsquared,rfFuncs$results$Variables)
ggplot(data = plot, aes(x = rfFuncs.results.Variables, y = rfFuncs.results.Rsquared)) + 
  geom_point(size = 4,color="red",alpha=0.5) + 
  geom_line(size = 1,color='black',alpha=0.5) + 
  labs(x = "Number of variables", y = expression("10-fold Cross-Validation"~R^2)) +
  theme_classic() +
  theme(axis.text=element_text(size=12,color = 'black'),axis.title=element_text(size=14,color = 'black'),
        axis.line.x=element_line(linetype=1,color="black",size=0),
        axis.line.y=element_line(linetype=1,color="black",size=0),
        axis.ticks.x=element_line(color="black",size=1,lineend = 2),
        axis.ticks.y=element_line(color="black",size=1,lineend = 2),
        panel.border = element_rect(linetype=1,color = "black", size = 1.5, fill = NA))

#step2##########################################################################

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

load(file = 'optvar51.Rdata')

set.seed(51)
dat <- read.csv(file = 'dat.csv',header = T)
dat2 <- na.omit(dat)
dat3 <- dat2[,-1]
x <- dat3[,optvar]
logBBB <- dat2$mean_otu_diversity
load(file = 'inTrain.Rdata')
trainx <- x[inTrain, ]
testx <- x[-inTrain, ]
trainy <- logBBB[inTrain]
testy <- logBBB[-inTrain]

cl <- makeCluster(16) 
registerDoParallel(cl)
for (ntree in c(500,800,1000,1200,1500,2000,2500)) {
  Sys.time()
  control <- trainControl(method="cv", number=10,search='grid',savePredictions = 'final', allowParallel = T)
  tunegrid <- expand.grid(.mtry = c(1,5,10,15,20,25,30,35,40,45,50,55,60))
  set.seed(51)
  custom <- train(trainx,trainy,method="rf", 
                  metric = "Rsquared",importance=T,
                  tuneGrid=tunegrid,ntree = ntree,trControl=control)
  
  save(custom,file = paste0('HPB-',ntree,".rdata"))
  Sys.time()
}
stopCluster(cl)

#step3##########################################################################

require(ggplot2)
library(data.table)

final_grid <- data.table()

load("HPB-500.rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(500,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)

load("HPB-800.rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(800,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)

load("HPB-1000.rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(1000,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)

load("HPB-1200.rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(1200,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)

load("HPB-1500.rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(1500,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)

load("HPB-2000.rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(2000,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)

load("HPB-2500.rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(2500,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)

final_grid$ntree[which.max(final_grid$Rsquared)]
final_grid$ntree[which.min(final_grid$RMSE)]

load("HPB-2500.rdata")
custom$finalModel

plot <- data.frame(custom$pred$pred,custom$pred$obs)
ggplot(data = plot, aes(x = custom.pred.obs, y = custom.pred.pred)) + 
  geom_point(size = 4,fill='red',color="black",alpha=0.4,shape=21) +
  labs(x = "Observed abundance", y = "Predicted abundance") +
  theme_classic() +
  theme(axis.text=element_text(size=12,color = 'black'),axis.title=element_text(size=14,color = 'black'),
        axis.line.x=element_line(linetype=1,color="black",size=0),
        axis.line.y=element_line(linetype=1,color="black",size=0),
        axis.ticks.x=element_line(color="black",size=1,lineend = 2),
        axis.ticks.y=element_line(color="black",size=1,lineend = 2),
        panel.border = element_rect(linetype=1,color = "black", size = 1.5, fill = NA)) +
  geom_smooth(formula = y ~ x, method = "lm",alpha=0.3,linetype = 0 ) +
  stat_smooth (formula = y ~ x,method = 'lm',geom="line",alpha=0.5, color="#9EC3DB",size=1.5)

#step4##########################################################################

predictRF <- predict(custom$finalModel,newdata = testx)

rsq <- function(x, y) summary(lm(y~x))$r.squared
rsq(testy, predictRF)
R2(testy, predictRF)

plot <- data.frame(predictRF,testy)
ggplot(data = plot, aes(x = testy, y = predictRF)) + 
  geom_point(size = 4,fill='red',color="black",alpha=0.4,shape=21) +
  labs(x = "Observed abundance", y = "Predicted abundance") +
  theme_classic() +
  theme(axis.text=element_text(size=12,color = 'black'),axis.title=element_text(size=14,color = 'black'),
        axis.line.x=element_line(linetype=1,color="black",size=0),
        axis.line.y=element_line(linetype=1,color="black",size=0),
        axis.ticks.x=element_line(color="black",size=1,lineend = 2),
        axis.ticks.y=element_line(color="black",size=1,lineend = 2),
        panel.border = element_rect(linetype=1,color = "black", size = 1.5, fill = NA)) +
  geom_smooth(formula = y ~ x, method = "lm",alpha=0.3,linetype = 0 ) +
  stat_smooth (formula = y ~ x,method = 'lm',geom="line",alpha=0.5, color="#9EC3DB",size=1.5)

#step5##########################################################################

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

load(file = 'optvar51.Rdata')

set.seed(51)
dat <- read.csv(file = 'dat.csv',header = T)
dat2 <- na.omit(dat)
dat3 <- dat2[,-1]
x <- dat3[,optvar]
logBBB <- dat2$mean_otu_diversity
load(file = 'inTrain.Rdata')
trainx <- x[inTrain, ]
testx <- x[-inTrain, ]
trainy <- logBBB[inTrain]
testy <- logBBB[-inTrain]

load("HPB-2500.rdata")

rf_gridsearch <- custom

set.seed(x)
cl <- makeCluster(16) 
registerDoParallel(cl)
control_grid <- trainControl(method = "cv", number = 10, search = "grid",savePredictions = 'final',allowParallel = T)
rfmodelx <- train(trainx,trainy, method='rf',tuneGrid=rf_gridsearch$bestTune, 
                  ntree= 2500, trControl=control_grid, 
                  metric = "Rsquared",impotance=T)
stopCluster(cl)
rfmodelx
predictRF_x <- predict(rfmodelx,newdata = testx)
rsq(testy, predictRF_x)

mapset <- read.csv(file = 'map.csv')
predictRF1 <- predict(rfmodel1,newdata = mapset)
predictRF2 <- predict(rfmodel2,newdata = mapset)
predictRF3 <- predict(rfmodel3,newdata = mapset)
predictRF4 <- predict(rfmodel4,newdata = mapset)
predictRF5 <- predict(rfmodel5,newdata = mapset)
predictRF6 <- predict(rfmodel6,newdata = mapset)
predictRF7 <- predict(rfmodel7,newdata = mapset)
predictRF8 <- predict(rfmodel8,newdata = mapset)
predictRF9 <- predict(rfmodel9,newdata = mapset)
predictRF10 <- predict(rfmodel10,newdata = mapset)

#step6##########################################################################

predictsum <- data.frame(predictRF1,predictRF2,predictRF3,predictRF4,predictRF5,
                         predictRF6,predictRF7,predictRF8,predictRF9,predictRF10,
                         mapset$lon,mapset$lat)

for (i in 1:nrow(predictsum)){
  predictsum$prediction_sd[i] <- sd(as.numeric(predictsum[i,1:10]))
  predictsum$prediction_mean[i] <- mean(as.numeric(predictsum[i,1:10]))
}
predictsum$cv <- predictsum$prediction_sd / predictsum$prediction_mean
write.csv(predictsum,file = 'pathogen.csv')

#step7##########################################################################

var6 <- varImp(rfmodel1, scale=FALSE)
var6$pRF <- var6$Overall / sum(var6$Overall) * 100
var6$rfround <- round(var6$pRF,2)

write.csv(var6,file = 'importance.csv')
