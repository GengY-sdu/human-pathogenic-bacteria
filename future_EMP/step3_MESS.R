#step3:MESS#####################################################################

rm(list = ls())
require(data.table)
require(raster)
require(dismo)
set.seed(1)
historial_clim <-  dir('pathogen/clim_1970_2000/',full.names = TRUE)|>raster::stack()
load(file = 'pathogen/future_EMP/MESS_dat.Rdata')
history_dataset <- MESS_dat
suppressWarnings(ms_report <- mess(historial_clim, history_dataset, full=TRUE))
plot(ms_report$mess)
result <- ms_report$mess|>as.data.frame(xy =TRUE)
result <- result[complete.cases(result),]
fwrite(result,"pathogen/future_EMP/MESS_table_nohuman.csv")
