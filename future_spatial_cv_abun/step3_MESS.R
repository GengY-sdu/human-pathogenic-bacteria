#step3:MESS#####################################################################

rm(list = ls())
require(data.table)
require(raster)
require(dismo)
set.seed(1)
historial_clim <-  dir('pathogen/clim_1970_2000/',full.names = TRUE)|>raster::stack()
history_dataset <- fread("pathogen/glm_start_table_nohuamn.csv",integer64 = "numeric")
history_dataset <- history_dataset[!history_dataset$otu_abun %in% boxplot.stats(history_dataset$otu_abun)$out,]
history_dataset <- history_dataset[,'mean_otu_abun':=mean(otu_abun),by = c("lat_lon1")]
history_dataset <- unique(history_dataset[,c(18,17,38,19:37)],by = c("y","x"))
history_dataset <- history_dataset[-which(history_dataset$mean_otu_abun=='0'),]
history_dataset <- history_dataset[,-c(1:3)]
history_dataset <- history_dataset[complete.cases(history_dataset),]
suppressWarnings(ms_report <- mess(historial_clim, history_dataset, full=TRUE))
plot(ms_report$mess)
result <- ms_report$mess|>as.data.frame(xy =TRUE)
result <- result[complete.cases(result),]
fwrite(result,"pathogen/future_spatial_cv_abun/MESS_table_nohuman_abun.csv")
