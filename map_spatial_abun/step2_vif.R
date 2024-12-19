#step2:vif######################################################################
rm(list = ls())
library(caret)
library(car)

load(file = 'pathogen/map_spatial_abun/df_unique_10.Rdata')
df <- df_unique
form_all2 <- as.formula(mean_otu_abun~.)
impVars <- df[,-1]

all_vifs_test <- try(vif(lm(form_all2, data=df)), silent=TRUE)
if (class(all_vifs_test) == "try-error"){
  lm_alias <- alias(lm(form_all2, data=df))
  broken <- data.frame(lm_alias$Complete)
  broken_var <- row.names(broken)
  nam_var <- names(impVars) %in% broken_var
  impVars <- impVars[!nam_var]
  form_all_new <- as.formula(paste("mean_otu_abun ~ ", 
                                   paste(names(impVars), collapse=" + "),
                                   sep=""))
  all_vifs <- vif(lm(form_all_new, data=df))
} else {
  all_vifs <- all_vifs_test
}

if(any(all_vifs > 10)){
  all_vifs <- as.data.frame(all_vifs)  
  while((nrow(all_vifs) > 2)& (max(all_vifs[, 1]) > 10)  &  
        (class(all_vifs) != "try-error")) {
    remove_var <- rownames(all_vifs)[which(all_vifs[, 1] == max(all_vifs[, 1]))]  
    impVars <- impVars[!names(impVars) %in% remove_var]  
    fullForm <- paste ("mean_otu_abun ~ ", paste (names(impVars), collapse=" + "), sep="")  
    fullMod <- lm(as.formula(fullForm), data=df)  
    all_vifs <- try(as.data.frame(vif(fullMod)), silent=TRUE) 
  }
  vif_filtered_variables <- names(fullMod$model)[!names(fullMod$model) %in% 
                                                   "mean_otu_abun"]
} else {
  all_vifs <- as.data.frame(all_vifs)
  vif_filtered_variables <- rownames(all_vifs)[!names(all_vifs) %in% "mean_otu_abun"]
}

df_filter <- df[,c("mean_otu_abun",vif_filtered_variables)]

save(df_filter,file = "pathogen/map_spatial_abun/ml_df_abun.Rdata")
