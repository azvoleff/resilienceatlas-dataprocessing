library(raster)
library(gdalUtils)
library(maptools)
library(stringr)
library(dplyr)
library(foreach)

prefix <- 'H:/Data'

water_mask <- readShapeSpatial('H:/Data/Global/Land_Water_Masks/Land.shp')

s3_folder <- file.path(prefix, 'Resilience_Atlas/S3_Buckets/cigrp/Climate')

###############################################################################
# Climate projections
cmip5_folder <- file.path(prefix, 'Resilience_Atlas', 'CMIP5')

# Precipitation projections
cmip5_pr_files <- list.files(cmip5_folder, pattern='^pr_')
cmip5_pr_out <- paste0(s3_folder, '/Rainfall/Projected/')

foreach(cmip5_pr_file=cmip5_pr_files) %do% {
    pr <- raster(file.path(cmip5_folder, cmip5_pr_file))
    pr[is.na(pr)] <- -9999
    pr <- mask(pr, water_mask, updatevalue=-9999,
               filename=file.path(cmip5_pr_out, cmip5_pr_file), 
               overwrite=TRUE)
}
