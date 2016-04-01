library(rgdal)
library(raster)
library(stringr)
library(gdalUtils)
library(rgeos)
library(teamlucc)
library(foreach)
library(lubridate)
library(doParallel)

cl <- makeCluster(4)
registerDoParallel(cl)

prefix <- 'H:/Data'

overwrite <- TRUE

product <- 'cru_ts3.23'
datestring <- '1901.2014'

in_folder <- file.path(prefix, "CRU", product)
out_folder <- file.path(prefix, "Resilience_Atlas", "CRU")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))

datasets <- c('tmn', 'tmx', 'pre')
#datasets <- c('tmp')

# This is the projection of the CRU files
s_srs <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'

# Note the below code is INCLUSIVE of the start date
cru_start_date <- as.Date('1901/1/1')
# Note the below code is EXCLUSIVE of the end date
cru_end_date <- as.Date('2015/1/1')
# Choose a start and end year for the data to include in this export
start_date <- as.Date('1985/1/1') # Inclusive
end_date <- as.Date('2015/1/1') # Exclusive

new_datestring <- paste(format(start_date, '%Y%m%d'), format(end_date-1, '%Y%m%d'), sep='-')

yrs <- seq(year(cru_start_date), year(cru_end_date))
dates <- seq(cru_start_date, cru_end_date, by='months')
dates <- dates[dates < cru_end_date]

included_dates <- dates[(dates >= start_date) & (dates < end_date)]
band_nums <- c(1:length(dates))[(dates >= start_date) & (dates < end_date)]

foreach (dataset=datasets, .inorder=FALSE,
         .packages=c("teamlucc", "rgeos", "raster", "rgdal")) %dopar% {
    timestamp()
    message('Processing ', dataset, '...')

    ncdf <- file.path(in_folder, dataset,
                      pattern=paste(product, datestring, dataset, 'dat.nc', 
                                    sep='.'))
    this_dataset <- stack(ncdf, bands=band_nums)
    proj4string(this_dataset) <- s_srs

    dstfile <- file.path(out_folder,
                          paste0("Global", "_", product, '_', dataset, '_', 
                                 new_datestring,  '.tif'))
    writeRaster(this_dataset, filename=dstfile, overwrite=TRUE)

    dstfile_mean <- file.path(out_folder,
                          paste0("Global", "_", product, '_', dataset, '_', 
                                 new_datestring,  '_mean.tif'))
    calc(this_dataset, function(x) {
             x <- mean(x)
             if(is.na(x)) {
                 return(-9999)
             } else {
                 return(x)
             }
        }, filename=dstfile_mean, overwrite=TRUE)
}
