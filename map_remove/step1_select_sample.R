#step1:select samples###########################################################
rm(list = ls())

dat <- read.csv(file = 'pathogen/map_remove/dat_remove.csv',header = T)
dat2 <- na.omit(dat)
colnames(dat2)[2] <- "longitude"
colnames(dat2)[3] <- "latitude"

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

save(df_unique,file = "pathogen/map_remove/df_unique_10.Rdata")
