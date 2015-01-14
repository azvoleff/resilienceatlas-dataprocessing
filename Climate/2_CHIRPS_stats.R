###############################################################################
# Calculates mean precipitation for GRP countries.
###############################################################################

source('../0_settings.R')

library(rgdal)
library(raster)
library(lubridate)
library(dplyr)
library(foreach)
library(doParallel)

n_cpus <- 25

cl  <- makeCluster(n_cpus)
registerDoParallel(cl)

product <- 'v1p8chirps'

# For monthly data:
dataset <- 'monthly' # For SPI, use monthly
date_limits_string <- '198101-201404'

# Note the below code is INCLUSIVE of the start date
chirps_start_date <- as.Date('1981/1/1')
# Note the below code is EXCLUSIVE of the end date
chirps_end_date <- as.Date('2014/5/1')

in_folder <- file.path(prefix, "GRP", "CHIRPS")
out_folder <- file.path(prefix, "GRP", "CHIRPS")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))

yrs <- seq(year(chirps_start_date), year(chirps_end_date))
dates <- seq(chirps_start_date, chirps_end_date, by='months')
dates <- dates[dates < chirps_end_date]
num_periods <- 12

# This is the projection of the CHIRPS files, read from the .hdr files 
# accompanying the data
s_srs <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'

ISO_2s <- c("ET", "DJ", "SO", "ER")

ISO_2 <- c("ET")

foreach (ISO_2=ISO_2s,
         .packages=c("rgdal", "lubridate", "dplyr", "raster")) %dopar% {
    timestamp()
    message('Processing ', ISO_2, '...')

    filename_base <- paste0(ISO_2, '_', product, '_', dataset, '_')

    chirps_tif_masked <- file.path(out_folder,
                            paste0(filename_base, date_limits_string, 
                                   '_NAs_masked.tif'))
    chirps <- brick(chirps_tif_masked)

    # Setup a dataframe with the precipitation data so anomalies, etc can be 
    # calculated
    years <- rep(yrs, each=num_periods)[1:nlayers(chirps)]
    years_rep <- rep(years, each=nrow(chirps)*ncol(chirps))
    subyears <- rep(seq(1, num_periods),  length.out=nlayers(chirps))
    subyears_rep <- rep(subyears, each=nrow(chirps)*ncol(chirps))
    pixels_rep <- rep(seq(1:(nrow(chirps)*ncol(chirps))), nlayers(chirps))
    chirps_df <- data.frame(year=years_rep,
                            subyear=subyears_rep, 
                            pixel=pixels_rep,
                            ppt=as.vector(chirps))
    chirps_df <- tbl_df(chirps_df)

    # Add 12 month total precip column. Use sides=1 in filter to have it be 12 
    # month total as of that date
    chirps_df <- group_by(chirps_df, pixel) %>%
        arrange(year, subyear) %>%
        mutate(tot_12mth=as.numeric(stats::filter(ppt, rep(1/12, 12), sides=1)*12),
               tot_24mth=as.numeric(stats::filter(ppt, rep(1/24, 24), sides=1)*24))

    # Calculate the mean_monthly for each subyear for each pixel
    ppt_mean_monthly <- group_by(chirps_df, pixel, subyear) %>%
        summarize(mean_monthly=mean(ppt, na.rm=TRUE))
    # Use chirps raster as a template
    ppt_mean_monthly_rast <- brick(chirps, values=FALSE, nl=num_periods)
    ppt_mean_monthly_rast <- setValues(ppt_mean_monthly_rast,
                               matrix(ppt_mean_monthly$mean_monthly, 
                                      nrow=nrow(chirps)*ncol(chirps), 
                                      ncol=num_periods, byrow=TRUE))
    writeRaster(ppt_mean_monthly_rast,
                filename=file.path(out_folder, paste0(filename_base, 
                                                      'ppt_mean_monthly.tif')), 
                overwrite=TRUE)

    # Calculate the mean annual precipitation for each pixel
    ppt_mean_12mth <- group_by(chirps_df, pixel, year) %>%
        summarize(total_annual=sum(ppt, na.rm=TRUE)) %>%
        group_by(pixel) %>%
        summarize(mean_annual=mean(total_annual, na.rm=TRUE))
    # Use chirps raster as a template
    ppt_mean_12mth_rast <- brick(chirps, values=FALSE, nl=1)
    ppt_mean_12mth_rast <- setValues(ppt_mean_12mth_rast,
                               matrix(ppt_mean_12mth$mean_annual, 
                                      nrow=nrow(chirps)*ncol(chirps), 
                                      ncol=1))
    writeRaster(ppt_mean_12mth_rast,
                filename=file.path(out_folder, paste0(filename_base, 
                                                      'ppt_mean_12mth.tif')), 
                overwrite=TRUE)

    # Calculate the mean annual precipitation for each pixel
    ppt_mean_24mth <- ppt_mean_12mth
    ppt_mean_24mth$mean_24mth <- ppt_mean_24mth$mean_annual * 2
    ppt_mean_24mth <- ppt_mean_24mth[names(ppt_mean_24mth) != "mean_annual"]
    # Use chirps raster as a template
    ppt_mean_24mth_rast <- brick(chirps, values=FALSE, nl=1)
    ppt_mean_24mth_rast <- setValues(ppt_mean_24mth_rast,
                               matrix(ppt_mean_24mth$mean_24mth, 
                                      nrow=nrow(chirps)*ncol(chirps), 
                                      ncol=1))
    writeRaster(ppt_mean_24mth_rast,
                filename=file.path(out_folder, paste0(filename_base, 
                                                      'ppt_mean_24mth.tif')), 
                overwrite=TRUE)

    chirps_df$anom_12mth <- chirps_df$tot_12mth - ppt_mean_12mth$mean_annual[match(chirps_df$pixel, ppt_mean_12mth$pixel)]
    chirps_df$anom_24mth <- chirps_df$tot_24mth - ppt_mean_24mth$mean_24mth[match(chirps_df$pixel, ppt_mean_24mth$pixel)]
    #filter(chirps_df, pixel == 1)[1:36,]

    save(chirps_df, file=file.path(out_folder, paste0(filename_base, 'ppt.RData')))

    # Save rasters of 12 and 24 month anomalies
    anom_12mth_rast <- brick(chirps, values=FALSE, nl=nlayers(chirps))
    anom_12mth_rast <- setValues(anom_12mth_rast,
                                 matrix(chirps_df$anom_12mth, 
                                        nrow=nrow(chirps)*ncol(chirps), 
                                        ncol=nlayers(chirps)))
    anom_12mth_rast_filename <- file.path(out_folder,
                                          paste0(filename_base, '12mth_anom.tif'))
    writeRaster(anom_12mth_rast, anom_12mth_rast_filename, overwrite=TRUE)

    anom_24mth_rast <- brick(chirps, values=FALSE, nl=nlayers(chirps))
    anom_24mth_rast <- setValues(anom_24mth_rast,
                                 matrix(chirps_df$anom_24mth, 
                                        nrow=nrow(chirps)*ncol(chirps), 
                                        ncol=nlayers(chirps)))
    anom_24mth_rast_filename <- file.path(out_folder,
                                          paste0(filename_base, '24mth_anom.tif'))
    writeRaster(anom_24mth_rast, anom_24mth_rast_filename, overwrite=TRUE)
}
