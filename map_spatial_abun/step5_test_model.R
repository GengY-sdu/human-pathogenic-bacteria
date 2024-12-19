#step5:test model###############################################################
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

set.seed(51)
load(file = 'pathogen/map_spatial_abun/optvar.Rdata')
load(file = 'pathogen/map_spatial_abun/ml_df_abun.Rdata')
dat <- df_filter
dat2 <- na.omit(dat)
dat3 <- dat2[,-1]
x <- dat3[,optvar]  
logBBB <- dat2$mean_otu_abun

load(file = 'pathogen/map_spatial_abun/inTrain.Rdata')
load(file = 'pathogen/map_spatial_abun/inTest.Rdata')
trainx <- x[inTrain, ]
testx <- x[inTest, ]
trainy <- logBBB[inTrain]
testy <- logBBB[inTest]

require(ggplot2)
library(data.table)

final_grid <- data.table()

load("pathogen/map_spatial_abun/HPB-500_abun.Rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(500,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)

load("pathogen/map_spatial_abun/HPB-800_abun.Rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(800,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)

load("pathogen/map_spatial_abun/HPB-1000_abun.Rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(1000,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)

load("pathogen/map_spatial_abun/HPB-1200_abun.Rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(1200,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)

load("pathogen/map_spatial_abun/HPB-1500_abun.Rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(1500,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)

load("pathogen/map_spatial_abun/HPB-2000_abun.Rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(2000,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)

load("pathogen/map_spatial_abun/HPB-2500_abun.Rdata")
grid_search <- data.table(custom$results)
grid_search$'ntree' <- rep(2500,nrow(grid_search))
final_grid <- dplyr::bind_rows(final_grid,grid_search)

write.csv(final_grid,file = 'pathogen/map_spatial_abun/grid_set_abun.csv',row.names = F)
final_grid$ntree[which.max(final_grid$Rsquared)]
final_grid$ntree[which.min(final_grid$RMSE)]

load("pathogen/map_spatial_abun/HPB-2500_abun.Rdata")
custom$finalModel

plot <- data.frame(custom$pred$pred,custom$pred$obs)
write.csv(plot,file = 'pathogen/map_spatial_abun/train_set_abun.csv',row.names = F)

predictRF <- predict(custom$finalModel,newdata = testx)

rsq <- function(x, y) summary(lm(y~x))$r.squared
rsq(testy, predictRF)
R2(testy, predictRF)

plot <- data.frame(predictRF,testy)
write.csv(plot,file = 'pathogen/map_spatial_abun/test_set_abun.csv',row.names = F)

