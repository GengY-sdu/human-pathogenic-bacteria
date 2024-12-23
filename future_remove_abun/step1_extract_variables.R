#step1:extract variables########################################################

rm(list = ls())
require(raster)
require(data.table)
require(pbapply)
require(doParallel)
require(foreach)
cl <- makeCluster(10)
registerDoParallel(cl)
historial_clim <-  dir('pathogen/clim_1970_2000/',full.names = TRUE)
total <- pblapply(historial_clim,function(x){
  df_0 <- data.table::fread("pathogen/future_remove/df_412_terra_nohuman.csv",integer64 = "numeric")
  n1 <- raster::raster(x)
  BIO_centroid <- raster::extract(n1, df_0[,c(2,1)], method='simple', buffer=1000, fun=mean, df=TRUE)
  return(BIO_centroid)},cl = cl)
stopCluster(cl)
mapped_df <- dplyr::bind_cols(total)
mapped_df <- mapped_df[,c(seq(2,38,2))]
df0 <- data.table::fread("pathogen/future_remove/df_412_terra_nohuman.csv",integer64 = "numeric")
mapped_df <- cbind(df0,mapped_df)
rm(df0,cl,total)
df0_add <- data.table::fread("pathogen/future_remove/df0_add_nohuman.csv",integer64 = "numeric")
mapped_df <- df0_add[mapped_df,on = c("lat_lon1")]
fwrite(mapped_df,"pathogen/glm_start_table_nohuamn_remove.csv")
rm(df0_add,mapped_df)
rm(historial_clim)
gc()