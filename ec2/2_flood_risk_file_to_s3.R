library(raster)

source('../0_settings.R')

regions <- c('SouthandSoutheastAsia', 'HornofAfrica', 'Sahel')
s3_folder <- file.path(prefix, 'GRP/S3_Buckets/cigrp')

###############################################################################
# Flood data from Hirabayashi et al.

#########################
# Precipitation (CHIRPS)
flood_folder <- file.path(prefix, 'GRP', 'Flood_Hirabayashi')

# Climatology
agreement_in <- file.path(flood_folder, 'MultAgreement-RCP85-20C100yr1.tif')
returnperiod_in <- file.path(flood_folder, 'MultReturnPeriod-RCP85-20C100yr1.tif')

agreement <- raster(agreement_in)
returnperiod <- raster(returnperiod_in)

returnperiod[returnperiod < 1] <- NA
returnperiod[is.nan(returnperiod)] <- NA
returnperiod[is.infinite(returnperiod)] <- NA

agreement_out <- paste0(s3_folder, '/Flood/RCP85_100yrflood_yr2100_returnperiod_multimodelagreement.tif')
writeRaster(agreement, filename=agreement_out, overwrite=TRUE, datatype='INT2S')

returnperiod_out <- paste0(s3_folder, '/Flood/RCP85_100yrflood_yr2100_returnperiod.tif')
writeRaster(returnperiod, filename=returnperiod_out, overwrite=TRUE, datatype='INT2S')
