#step4:test model###############################################################
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
library(blockCV)

load(file = 'pathogen/map_spatial_cv/optvar.Rdata')
load(file = 'pathogen/map_spatial_cv/ml_df.Rdata')
dat <- df_filter
dat2 <- na.omit(dat)
dat3 <- dat2[,-1]
x <- dat3[,optvar]   
logBBB <- dat2$mean_otu_diversity

i <- 1

set.seed(i)
load(file = 'pathogen/map_spatial_cv/inTrain.Rdata')
trainx <- x[inTrain, ]
testx <- x[-inTrain, ]
trainy <- logBBB[inTrain]
testy <- logBBB[-inTrain]

require(ggplot2)
library(data.table)

final_grid <- data.table()

load("pathogen/map_spatial_cv/HPB-500.Rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(500,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)

load("pathogen/map_spatial_cv/HPB-800.Rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(800,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)

load("pathogen/map_spatial_cv/HPB-1000.Rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(1000,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)

load("pathogen/map_spatial_cv/HPB-1200.Rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(1200,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)

load("pathogen/map_spatial_cv/HPB-1500.Rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(1500,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)

load("pathogen/map_spatial_cv/HPB-2000.Rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(2000,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)

load("pathogen/map_spatial_cv/HPB-2500.Rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(2500,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)

write.csv(final_grid,file = 'pathogen/map_spatial_cv/grid_cv.csv',row.names = F)
final_grid$ntree[which.max(final_grid$Rsquared)]
final_grid$ntree[which.min(final_grid$RMSE)]

load("pathogen/map_spatial_cv/HPB-1200.Rdata")
custom$finalModel

plot <- data.frame(custom$pred$pred,custom$pred$obs)
write.csv(plot,file = 'pathogen/map_spatial_cv/train_cv.csv',row.names = F)

predictRF <- predict(custom$finalModel,newdata = testx)

rsq <- function(x, y) summary(lm(y~x))$r.squared
rsq(testy, predictRF)
R2(testy, predictRF)

plot <- data.frame(predictRF,testy)
write.csv(plot,file = 'pathogen/map_spatial_cv/test_cv.csv',row.names = F)
