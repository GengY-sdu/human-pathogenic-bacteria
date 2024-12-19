#step2:data process#############################################################
rm(list = ls())
require(data.table)
history_dataset <- fread("pathogen/glm_start_table_nohuamn.csv",integer64 = "numeric")
history_dataset <- history_dataset[!history_dataset$otu_diversity %in% boxplot.stats(history_dataset$otu_diversity)$out,]
history_dataset <- history_dataset[,'mean_otu_diversity':=mean(otu_diversity),by = c("lat_lon1")]
history_dataset <- unique(history_dataset[,c(18,17,38,19:37)],by = c("y","x"))
history_dataset <- history_dataset[-which(history_dataset$mean_otu_diversity=='0'),]
history_dataset <- history_dataset[complete.cases(history_dataset),]
location <- history_dataset[,c(1:3)]
save(location,file = 'pathogen/future_spatial_cv/location.Rdata')
history_dataset <- history_dataset[,-c(1:2)]
colnames(history_dataset)[2:20] <- colnames(history_dataset)[2:20]|>stringr::str_extract("(?<=wc2.1_5m_).*")
colnames(history_dataset)[2:20] <- colnames(history_dataset)[2:20]|>stringr::str_replace("_","")
history_dataset <- history_dataset[,c(1,2,13:20,3:12)]
colnames(history_dataset)[2:10] <- paste0("bio0",1:9)
colnames(history_dataset)
save(history_dataset,file = 'pathogen/future_spatial_cv/history_dataset_nohuman.Rdata')









