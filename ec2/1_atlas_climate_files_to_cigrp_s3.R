library(raster)
library(gdalUtils)
library(stringr)
library(dplyr)
library(foreach)

source('../0_settings.R')

regions <- c('SouthandSoutheastAsia', 'HornofAfrica', 'Sahel')

s3_folder <- 'H:/Data/GRP/S3_Buckets/cigrp/Climate'

###############################################################################
# Historical data (climatology and trends)

#########################
# Precipitation (CHIRPS)
chirps_folder <- file.path(prefix, 'GRP', 'CHIRPS-2.0')

# Climatology
chirps_monthlytotal_in <- paste0(regions, '_CHIRPS_monthly_198101-201412_mean_monthly.gri')
chirps_monthlytotal_in <- file.path(chirps_folder, chirps_monthlytotal_in)
chirps_monthlytotal_out <- paste0(s3_folder, '/Rainfall/Historical/CHIRPS_19850101-20141201_monthlyppt_total_mm.tif')
vrtfile <- tempfile(fileext='.vrt')
gdalbuildvrt(chirps_monthlytotal_in, vrtfile, vrtnodata=-9999)
chirps_monthlytotal_mosaic <- mosaic_rasters(vrtfile,
                                             chirps_monthlytotal_out,
                                             output_Raster=TRUE)

chirps_annualtotal_mosaic <- calc(chirps_monthlytotal_mosaic, fun=sum, filename=paste0(s3_folder, '/Rainfall/Historical/CHIRPS_19850101-20141201_annualppt_total_mm.tif'), overwrite=TRUE)

# Trend
# TODO: NAs are not showing properly for this layer
chirps_trend_in <- paste0(regions, '_CHIRPS_monthly_19850101-20141201_trend_annual.gri')
chirps_trend_in <- file.path(chirps_folder, chirps_trend_in)
chirps_trend_out <- paste0(s3_folder, '/Rainfall/Historical/CHIRPS_19850101-20141201_annualppt_trend_pct-per-decade.tif')
vrtfile <- tempfile(fileext='.vrt')
gdalbuildvrt(chirps_trend_in, vrtfile, vrtnodata=-9999)
chirps_trend_mosaic <- mosaic_rasters(vrtfile,
                                      chirps_trend_out,
                                      output_Raster=TRUE)

#########################
# Temperature (CRU)
cru_folder <- file.path(prefix, 'GRP', 'CRU')

# Climatology
cru_monthlymean_in <- paste0(regions, '_cru_ts3.23_tmp_1901.2014_mean_monthly.gri')
cru_monthlymean_in <- file.path(cru_folder, cru_monthlymean_in)
cru_monthlymean_out <- paste0(s3_folder, '/Temperature/Historical/cru_ts3.23_19850101-20141201_tmp_monthlymean_deg.tif')
vrtfile <- tempfile(fileext='.vrt')
gdalbuildvrt(cru_monthlymean_in, vrtfile, vrtnodata=-9999)
cru_monthlymean_mosaic <- mosaic_rasters(vrtfile,
                                         cru_monthlymean_out,
                                         output_Raster=TRUE)

cru_annualmean_mosaic <- calc(cru_monthlymean_mosaic, fun=mean, filename=paste0(s3_folder, 'cru_ts3.23_19850101-20141201_tmp_annualmean_deg.tif'), overwrite=TRUE)


# Trend
cru_tmp_trend_in <- paste0(regions, '_cru_ts3.23_tmp_decadal_slope.tif')
cru_tmp_trend_in <- file.path(cru_folder, cru_tmp_trend_in)
cru_tmp_trend_out <- paste0(s3_folder, '/Temperature/Historical/cru_ts3.23_19850101-20141201_tmp_trend_deg-per-decade.tif')
cru_tmp_trend_mosaic <- mosaic_rasters(cru_tmp_trend_in,
                                       cru_tmp_trend_out,
                                       output_Raster=TRUE)

###############################################################################
# Climate projections
cmip5_folder <- file.path(prefix, 'GRP', 'CMIP5')

# Precipitation projections
cmip5_pr_files <- list.files(cmip5_folder, pattern='^pr_')
cmip5_pr_out <- paste0(s3_folder, '/Rainfall/Projected/')
file.copy(file.path(cmip5_folder, cmip5_pr_files), cmip5_pr_out)

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
        this_pair <- filter(temp_file_table,
                            scenario == this_scenario, period == this_period)
        stopifnot(nrow(this_pair) == 2)
        r1 <- raster(file.path(cmip5_folder, this_pair$file[1]))
        r2 <- raster(file.path(cmip5_folder, this_pair$file[2]))
        out_file <- paste0(cmip5_temp_out, paste0('/meandailytemp_', 
                                                  this_scenario, '_', 
                                                  this_period, '_', 
                                                  this_season, '.tif'))
        temp <- overlay(stack(r1, r2), fun=mean, filename=out_file, 
                        overwrite=TRUE)
}
