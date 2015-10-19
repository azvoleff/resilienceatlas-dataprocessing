###############################################################################
# Calculates mean precipitation for GRP countries.
###############################################################################

source('../0_settings.R')

library(rgdal)
library(stringr)
library(raster)
library(tools)
library(dplyr)
library(foreach)
library(abind)
library(spatial.tools)
library(doParallel)
library(reshape2)

cl  <- makeCluster(24)
registerDoParallel(cl)

in_folder <- file.path(prefix, "GRP", "CHIRPS-2.0")
out_folder <- file.path(prefix, "GRP", "CHIRPS-2.0")
# in_folder <- file.path(prefix, "Vital_Signs", "CHIRPS-2.0")
# out_folder <- file.path(prefix, "Vital_Signs", "CHIRPS-2.0")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))

datafiles <- dir(in_folder, pattern='_CHIRPS_monthly_198101-201412.tif$')

# What periods should anomalies be calculated over?
anom_periods <- c(3, 6, 12)

# What periods should mean monthly totals be calculated over?
mean_monthly_periods <- c('198501-201412', '198501-199912', '200012-201412')

print('Processing mean total monthly precips...')
foreach (datafile=datafiles) %:% 
    foreach(mean_monthly_period=mean_monthly_periods) %do% {

    timestamp()
    name <- str_extract(datafile, '^[a-zA-Z]*')
    print(paste0("Processing ", name, ", ", mean_monthly_period, "..."))

    # Calculate which layers to include, per mean_monthly_period
    file_start_date <- as.Date(paste0(str_extract(datafile, '(?<=_)[0-9]{6}'), '01'), '%Y%m%d')
    file_end_date <- as.Date(paste0(str_extract(datafile, '(?<=-)[0-9]{6}(?=.)'), '01'), '%Y%m%d')
    file_dates <- seq(file_start_date, file_end_date, by='month')

    period_start <- str_extract(mean_monthly_period, '^[0-9]*(?=-)')
    period_start_date <- as.Date(paste0(period_start, '01'), '%Y%m%d')
    period_end <- str_extract(mean_monthly_period, '(?<=-)[0-9]*')
    period_end_date <- as.Date(paste0(period_end, '01'), '%Y%m%d')
    period_dates <- seq(period_start_date, period_end_date, by='month')

    out_basename <- file.path(in_folder, file_path_sans_ext(datafile))
    out_basename <- gsub('(?<=_)[0-9]{6}', period_start, out_basename, perl=TRUE)
    out_basename <- gsub('(?<=-)[0-9]{6}', period_end, out_basename, perl=TRUE)

    chirps <- stack(file.path(in_folder, datafile), bands=which(file_dates %in% period_dates))

    calc_monthly_mean <- function(p, ...) {
        p[p == -9999] <- NA
        mthly_mean <- foreach(n=1:12, .combine=c, .final=raster::stack) %do%{
            # Pull out all arrays for this month and calculate mean
            raster::mean(p[[seq(from=n, by=12, length.out=round(dim(p)[3]/12))]])
        }
        # Note that tranpose=TRUE is needed as rasterEngine uses cols, rows, 
        # bands ordering
        as.array(mthly_mean, transpose=TRUE)
    }
    mthly_mean <- rasterEngine(p=chirps, fun=calc_monthly_mean,
        datatype='FLT4S', outbands=12, outfiles=1, processing_unit="chunk", 
        chunk_format="raster", filename=paste0(out_basename, '_mean_monthly'),
        .packages='raster')
}
timestamp()
print('Finished processing mean total monthly precips.')

# Function to calculate running total precip over a particular period
calc_runtotal_n_mth <- function(p, n_mth, ...) {
    p[p == -9999] <- NA
    out <- apply(p, c(1, 2), FUN=function(x) {stats::filter(x, rep(1, n_mth), sides=1)})
    out <- aperm(out, c(2, 3, 1))
    # rasterEngine does not properly handle NAs, so recode these to -32767
    out[is.na(out)] <- -32767
    out
}

# Function to calculate precip anomaly over a particular period
calc_anom_n_mth <- function(p, n_mth, ...) {
    p[p == -9999] <- NA
    # Calculate running total precip for periods of length n_mth
    runtot <- apply(p, c(1, 2), FUN=function(x) {
        stats::filter(x, rep(1, n_mth), sides=1)
        })
    runtot <- aperm(runtot, c(2, 3, 1))
    # Calculate the mean precip for each pixel for each of these periods 
    period_mean <- foreach(n=1:12, .combine=abind, .export='runtot') %do% {
        out <- apply(runtot[ , , seq(from=n, by=12, length.out=round(dim(p)[3]/12))], c(1, 2), mean)
        out <- array(out, dim=c(dim(runtot)[1], dim(runtot)[2], 1))
    }
    # Subtract the mean for each period from the running total to get 
    # anomalies calculated over a period equal to n_mth. Note that each 
    # month in the year has a different mean value subtracted as the mean 
    # precipitation is calculated for each period to allow for seasonal 
    # variation in total precip.
    anom <- sweep(runtot, c(1, 2), period_mean, check.margin=FALSE)
    # rasterEngine does not properly handle NAs, so recode these to -32767
    anom[is.na(anom)] <- -32767
    anom
}

print('Processing precip anomalies...')
foreach (datafile=datafiles) %:% foreach(anom_period=anom_periods) %do% {
    timestamp()
    # Calculate running totals and anomalies for selected anomaly periods
    runtotal <- rasterEngine(p=chirps, args=list(n_mth=anom_period),
        fun=calc_runtotal_n_mth, outbands=nlayers(chirps), datatype='INT2S', 
        processing_unit="chunk", outfiles=1,
        filename=paste0(out_basename, '_runtotal_', anom_period, 'mth'))

    anom <- rasterEngine(p=chirps, args=list(n_mth=anom_period),
        fun=calc_anom_n_mth, outbands=nlayers(chirps), datatype='INT2S', 
        processing_unit="chunk", outfiles=1, .packages=c('abind'),
        filename=paste0(out_basename, '_anom_', anom_period, 'mth'))
}
timestamp()
print('Finished processing precip anomalies...')
# plot(mthly_mean)
# plot(mthly_mean, zlim=c(0,450))
#
# plot(anom[[45]], zlim=c(-600,600))
# plot(anom[[46]], zlim=c(-600,600))
# plot(anom[[47]], zlim=c(-600,600))
# plot(anom[[63]], zlim=c(-600,600))
# plot(anom[[64]], zlim=c(-600,600))
# plot(anom[[65]], zlim=c(-600,600))
#
# plot(runtotal[[45]])
# plot(runtotal[[46]])
# plot(runtotal[[47]])
# plot(runtotal[[63]])
# plot(runtotal[[64]])
# plot(runtotal[[65]])
