library(raster)
library(gdalUtils)
library(stringr)
library(dplyr)
library(foreach)

source('../0_settings.R')

regions <- c('SouthandSoutheastAsia', 'HornofAfrica', 'Sahel')
s3_folder <- file.path(prefix, 'GRP/S3_Buckets/cigrp/Climate')

###############################################################################
# Historical data (climatology and trends)

#########################
# Precipitation (CHIRPS)
chirps_folder <- file.path(prefix, 'GRP', 'CHIRPS-2.0')

# Climatology
chirps_monthlytotal_in <- paste0(regions, '_CHIRPS_monthly_198101-201412_mean_monthly.gri')
chirps_monthlytotal_in <- file.path(chirps_folder, chirps_monthlytotal_in)
vrtfile <- tempfile(fileext='.vrt')
gdalbuildvrt(chirps_monthlytotal_in, vrtfile, vrtnodata=-9999)
chirps_monthlytotal_mosaic_tmp <- mosaic_rasters(vrtfile,
                                             tempfile(fileext='.tif'),
                                             output_Raster=TRUE)
chirps_annualtotal_mosaic_tmp <- calc(chirps_monthlytotal_mosaic_tmp,
                                      fun=sum, 
                                      filename=tempfile(fileext='.tif'), 
                                      overwrite=TRUE)

# Now mask areas of zero ppt (these are outside dataset)
ppt_mask <- chirps_annualtotal_mosaic_tmp == 0
chirps_monthlytotal_out <- paste0(s3_folder, '/Rainfall/Historical/CHIRPS_19850101-20141201_monthlyppt_total_mm.tif')
chirps_monthlytotal_mosaic <- mask(chirps_monthlytotal_mosaic_tmp, 
                                   ppt_mask, filename=chirps_monthlytotal_out, 
                                   overwrite=TRUE, maskvalue=1)
chirps_annualtotal_out <- paste0(s3_folder, '/Rainfall/Historical/CHIRPS_19850101-20141201_annualppt_total_mm.tif')
chirps_annualtotal_mosaic <- mask(chirps_annualtotal_mosaic_tmp, 
                                  ppt_mask,
                                  filename=chirps_annualtotal_out, 
                                  overwrite=TRUE)

# Trend
chirps_trend_in <- paste0(regions, '_CHIRPS_monthly_19850101-20141201_trend_annual.gri')
chirps_trend_in <- file.path(chirps_folder, chirps_trend_in)
vrtfile <- tempfile(fileext='.vrt')
gdalbuildvrt(chirps_trend_in, vrtfile, vrtnodata=-9999)
chirps_trend_mosaic_tmp  <- mosaic_rasters(vrtfile,
                                           tempfile(fileext='.tif'),
                                           b=1,
                                           output_Raster=TRUE)
chirps_trend_out <- paste0(s3_folder, '/Rainfall/Historical/CHIRPS_19850101-20141201_annualppt_trend_pct-per-decade.tif')
chirps_trend_mosaic <- mask(chirps_trend_mosaic_tmp, ppt_mask, 
                            filename=chirps_trend_out, overwrite=TRUE)

# Interannual variability
chirps_cv_in <- paste0(regions, '_CHIRPS_monthly_198501-201412_interannualvariability_pct.tif')
chirps_cv_in <- file.path(chirps_folder, chirps_cv_in)
vrtfile <- tempfile(fileext='.vrt')
gdalbuildvrt(chirps_cv_in, vrtfile, vrtnodata=-9999)
chirps_cv_mosaic_tmp  <- mosaic_rasters(vrtfile,
                                        tempfile(fileext='.tif'),
                                        output_Raster=TRUE)
chirps_cv_out <- paste0(s3_folder, '/Rainfall/Historical/CHIRPS_monthly_198501-201412_interannualvariability_pct.tif')
chirps_cv_mosaic <- mask(chirps_cv_mosaic_tmp, ppt_mask, 
                         filename=chirps_cv_out, overwrite=TRUE)


# Seasonal trends
file_key <- read.csv(file.path(chirps_folder, 'seasonal_precipitation_totals.csv'))
file_key$season <- gsub(' ', '-', file_key$season)
file_key$filename <- file.path(chirps_folder, 
                               paste0('CHIRPS_monthly_19850101-20141201_', 
                                      file_key$ISO3, '_trend_decadal_', 
                                      file_key$season, '.gri'))
file_key <- group_by(file_key, ISO3) %>%
    mutate(season_num=order(ppt, decreasing=TRUE))

foreach(this_season_num=unique(file_key$season_num)) %do% {
    base <- ppt_mask
    base[] <- NA
    chirps_seasonal_trend_files <- filter(file_key, season_num == this_season_num)$filename
    foreach (chirps_seasonal_trend_file=chirps_seasonal_trend_files) %do% {
        print(chirps_seasonal_trend_file)
        r <- raster(chirps_seasonal_trend_file)
        r <- extend(r, base)
        # Mask areas of r that are already in base
        r[!is.na(base)] <- NA
        # Set areas where r has data that base doesn't to zero in base
        base[!is.na(r) & is.na(base)] <- 0
        # Remove NAs from r before addition so that they don't get transferred to 
        # base
        r[is.na(r)] <- 0
        base <- base + r
    }
    writeRaster(base, filename=paste0(s3_folder, 
                                      '/Rainfall/Historical/CHIRPS_mon_trnd_dec_rainyseas', 
                                      this_season_num, '.tif'), overwrite=TRUE)
}


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
                                         tempfile(fileext='.tif'),
                                         output_Raster=TRUE)
# GDAL nodata coding isn't working, so rewrite with writeRaster
writeRaster(cru_monthlymean_mosaic, cru_monthlymean_out, overwrite=TRUE)

cru_annualmean_mosaic <- calc(cru_monthlymean_mosaic, fun=mean, filename=paste0(s3_folder, '/Temperature/Historical/cru_ts3.23_19850101-20141201_tmp_annualmean_deg.tif'), overwrite=TRUE)



# Trend
cru_tmp_trend_in <- paste0(regions, '_cru_ts3.23_tmp_decadal_slope.tif')
cru_tmp_trend_in <- file.path(cru_folder, cru_tmp_trend_in)
cru_tmp_trend_out <- paste0(s3_folder, '/Temperature/Historical/cru_ts3.23_19850101-20141201_tmp_trend_deg-per-decade.tif')
vrtfile <- tempfile(fileext='.vrt')
gdalbuildvrt(cru_tmp_trend_in, vrtfile, vrtnodata=-9999)
cru_tmp_trend_mosaic <- mosaic_rasters(vrtfile,
                                       tempfile(fileext='.tif'),
                                       b=1,
                                       output_Raster=TRUE)
# GDAL nodata coding isn't working, so rewrite with writeRaster
writeRaster(cru_tmp_trend_mosaic, cru_tmp_trend_out, overwrite=TRUE)

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
