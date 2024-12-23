# Anthropogenic activity and climate change exacerbate the spread of pathogenic bacteria in the environment

## Introduction

This project includes all R codes for machine learning and climate change analysis of human pathogenic bacteria (HPB).

Please place these files in a folder named `pathogen` and set default working directory of R to the previous level of the `pathogen`. For example, if the path of `pathogen` is `a/b/c/pthogen`, execute the command `setwd(dir = "a/b/c/")` in R.

## Detailed description

-   Folder ending in `spatial` or `spatial_abun`: Split the training and testing sets that are spatially distinct using the `blockCV` package. Among them, `map_spatial` and `map_spatial_abun` are used to predict the global distribution of HPB richness and abundance. `future_spatial` and `future_spatial_abun` are used to analyze the impact of climate change on HPB.

-   Folder ending in `spatial_cv` or `spatial_cv_abun`: Apply spatial cross-validation through the `blockCV` package. Among them, `map_spatial_cv` and `map_spatial_cv_abun` are used to predict the global distribution of HPB richness and abundance. `future_spatial_cv` and `future_spatial_cv_abun` are used to analyze the impact of climate change on HPB.

-   Folder ending in `remove` or `remove_abun`: Remove the commensal. Among them, `map_remove` and `map_remove_abun` are used to predict the global distribution of richness and abundance of HPB excluding commensal. `future_remove` and `future_remove_abun` are used to analyze the impact of climate change on HPB excluding commensal.

-   Folder ending in `EMP`: Analyze using the Earth Microbiome Project (EMP). Among them, `map_remove` and `map_EMP` are used to predict the global distribution of HPB. `future_EMP` are used to analyze the impact of climate change on HPB.

## Please note

Please download the map data for analysis from xx.
