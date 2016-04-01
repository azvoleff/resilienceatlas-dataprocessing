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

# Temperature projections - need to calculate mean of min and max layers
cmip5_temp_files <- list.files(cmip5_folder, pattern='^tas(min)|(max)')
temp_file_table <- data.frame(file=cmip5_temp_files,
                              scenario=str_extract(cmip5_temp_files, 'rcp((45)|(85))'),
                              variable=str_extract(cmip5_temp_files, '^(tasmin)|(tasmax)'),
                              period=str_extract(cmip5_temp_files, '[0-9]{4}-[0-9]{4}_vs_[0-9]{4}-[0-9]{4}'),
                              season=str_extract(cmip5_temp_files, '[a-zA-Z0-9]*(?=.tif)'))

cmip5_temp_out <- paste0(s3_folder, '/Temperature/Projected/')
foreach(this_scenario=unique(temp_file_table$scenario)) %:%
    foreach(this_period=unique(temp_file_table$period)) %:%
    foreach(this_season=unique(temp_file_table$season)) %do% {
        print(paste('Processing:', this_scenario, this_period, this_season))
        this_pair <- filter(temp_file_table,
                            scenario == this_scenario, period == this_period)
        stopifnot(nrow(this_pair) == 2)
        r1 <- raster(file.path(cmip5_folder, this_pair$file[1]))
        r2 <- raster(file.path(cmip5_folder, this_pair$file[2]))
        out_file <- paste0(cmip5_temp_out, paste0('/meandailytemp_', 
                                                  this_scenario, '_', 
                                                  this_period, '_', 
                                                  this_season, '.tif'))
        temp <- overlay(stack(r1, r2), fun=mean)
        temp[is.na(temp)] <- -9999
        temp <- mask(temp, water_mask, updatevalue=-9999, filename=out_file, 
                     overwrite=TRUE)
}
