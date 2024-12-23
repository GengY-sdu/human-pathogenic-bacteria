#step2:data process#############################################################

rm(list = ls())
require(data.table)
history_dataset <- fread("pathogen/future_EMP/glm_start_table_nohuamn_EMP.csv",integer64 = "numeric")
history_dataset <- history_dataset[,'mean_otu_diversity':=mean(otu_diversity),by = c("lat_lon1")]
history_dataset <- unique(history_dataset[,c(18,17,38,19:37)],by = c("y","x"))
history_dataset <- history_dataset[-which(history_dataset$mean_otu_diversity=='0'),]
history_dataset <- history_dataset[complete.cases(history_dataset),]

dat <- history_dataset
dat2 <- na.omit(dat)
colnames(dat2)[1] <- "longitude"
colnames(dat2)[2] <- "latitude"

df <- dat2
library(geosphere)
remove_nearby_points <- function(df, max_distance_m = 10) {
  result <- df[1, ]
  for (i in 2:nrow(df)) {
    keep_point <- TRUE
    for (j in 1:nrow(result)) {
      dist <- distVincentySphere(c(df$longitude[i], df$latitude[i]), c(result$longitude[j], result$latitude[j]))
      if (dist < max_distance_m) {
        keep_point <- FALSE  
        break
      }
    }
    if (keep_point) {
      result <- rbind(result, df[i, ]) 
    }
  }
  return(result)
}
df_unique <- remove_nearby_points(df, max_distance_m = 10)

history_dataset <- df_unique
history_dataset <- history_dataset[,-c(1:2)]
colnames(history_dataset)[2:20] <- colnames(history_dataset)[2:20]|>stringr::str_extract("(?<=wc2.1_5m_).*")
colnames(history_dataset)[2:20] <- colnames(history_dataset)[2:20]|>stringr::str_replace("_","")
history_dataset <- history_dataset[,c(1,2,13:20,3:12)]
colnames(history_dataset)[2:10] <- paste0("bio0",1:9)
colnames(history_dataset)
save(history_dataset,file = 'pathogen/future_EMP/history_dataset_nohuman.Rdata')



