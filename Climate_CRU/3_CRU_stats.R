###############################################################################
# Calculates mean annual precipitation/temperature and interannual variability
###############################################################################

source('../0_settings.R')

library(rgdal)
library(raster)
library(lubridate)
library(dplyr)
library(foreach)
library(doParallel)
library(spatial.tools)
library(tools)

n_cpus <- 3

cl  <- makeCluster(n_cpus)
registerDoParallel(cl)

product <- 'cru_ts3.23'
datestring <- '1901.2014'

# Note the below code is INCLUSIVE of the start date
cru_start_date <- as.Date('1901/1/1')
# Note the below code is EXCLUSIVE of the end date
cru_end_date <- as.Date('2015/1/1')

yrs <- seq(year(cru_start_date), year(cru_end_date))
dates <- seq(cru_start_date, cru_end_date, by='months')
dates <- dates[dates < cru_end_date]
num_periods <- 12

# Choose a start and end year for the data to include in this analysis
start_date <- as.Date('1985/1/1') # Inclusive
end_date <- as.Date('2015/1/1') # Exclusive

#datasets <- c('tmp', 'tmn', 'tmx', 'pre')
datasets <- c('tmp')

# out_folder <- file.path(prefix, "GRP", "CRU")
# shp_folder <- file.path(prefix, "GRP", "Boundaries")
out_folder <- file.path(prefix, "Vital_Signs", "CRU")
shp_folder <- file.path(prefix, "Vital_Signs", "Boundaries")
stopifnot(file_test('-d', out_folder))
stopifnot(file_test('-d', shp_folder))

aoi_polygons <- readOGR(shp_folder, 'Region_Hulls')

foreach (dataset=datasets, .inorder=FALSE,
         .packages=c("rgdal", "lubridate", "dplyr", "raster")) %:%
    foreach (n=1:nrow(aoi_polygons), .inorder=FALSE) %do% {

    aoi <- aoi_polygons[n, ]
    name <- as.character(aoi$Region_Nam)
    name <- gsub(' ', '', name)

    in_file <- file.path(out_folder,
                          paste0(name, "_", product, '_', dataset, '_', 
                                 datestring,  '.tif'))
    out_base <- file_path_sans_ext(in_file)

    # Calculate the band numbers that are needed
    included_dates <- dates[(dates >= start_date) & (dates < end_date)]
    band_nums <- c(1:length(dates))[(dates >= start_date) & (dates < end_date)]

    cru_data <- stack(in_file, bands=band_nums)

    calc_monthly_mean <- function(d, ...) {
        d[d == -9999] <- NA
        mthly_mean <- foreach(n=1:12, .combine=c, .final=raster::stack) %do%{
            # Pull out all arrays for this month and calculate mean
            raster::mean(d[[seq(from=n, by=12, length.out=round(dim(d)[3]/12))]])
        }
        # Note that tranpose=TRUE is needed as rasterEngine uses cols, rows, 
        # bands ordering
        as.array(mthly_mean, transpose=TRUE)
    }
    mthly_mean <- rasterEngine(d=cru_data, fun=calc_monthly_mean,
        datatype='FLT4S', outbands=12, outfiles=1, processing_unit="chunk", 
        chunk_format="raster", filename=paste0(out_base, '_mean_monthly'),
        .packages='raster', debugmode=FALSE)
}
stopCluster(cl)
