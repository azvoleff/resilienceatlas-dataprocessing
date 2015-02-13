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

# cl  <- makeCluster(3)
# registerDoParallel(cl)


###### TEMPORARY
# # For monthly data:
# dataset <- 'monthly' # For SPI, use monthly
dataset <- 'v1p8chirps_monthly' # For SPI, use monthly
###### TEMPORARY

in_folder <- file.path(prefix, "GRP", "CHIRPS")
out_folder <- file.path(prefix, "GRP", "CHIRPS")
shp_folder <- file.path(prefix, "GRP", "Boundaries", "Regional")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))
stopifnot(file_test('-d', shp_folder))

###### TEMPORARY
# # Note the below code is INCLUSIVE of the start date
# chirps_start_date <- as.Date('1981/1/1')
# # Note the below code is INCLUSIVE of the end date
# chirps_end_date <- as.Date('2014/12/1')
chirps_start_date <- as.Date('1981/1/1')
chirps_end_date <- as.Date('2014/4/1')
###### TEMPORARY

yrs <- seq(year(chirps_start_date), year(chirps_end_date))
dates <- seq(chirps_start_date, chirps_end_date, by='months')
periods_per_year <- 12

###### TEMPORARY
# # Select the start and end dates for the data to include in this analysis
# start_date <- as.Date('1985/1/1') # Inclusive
# end_date <- as.Date('2014/12/1') # Exclusive
start_date <- as.Date('1984/1/1')
end_date <- as.Date('2013/12/1')
###### TEMPORARY

region_polygons <- readOGR(shp_folder, 'GRP_regions')

#region_rows <- c(2, 3, 5)
region_rows <- c(1)

foreach (n=region_rows, .inorder=FALSE,
         .packages=c('raster', 'rgeos', 'dplyr', 'lubridate',
                     'rgdal')) %do% {
    timestamp()
    aoi <- region_polygons[n, ]
    region <- as.character(aoi$Region)
    region <- gsub(' ', '', region)

    print(paste0("Processing ", region, "..."))

    ###### TEMPORARY
    in_basename <- file.path(in_folder, paste0(region, ''))
    #in_basename <- file.path(in_folder, paste0(region, '_CHIRPS'))
    ###### TEMPORARY

    out_basename <- file.path(out_folder, paste0(region, '_CHIRPS_',
                                format(start_date, "%Y%m"), '-', 
                                format(end_date, "%Y%m")))
                                  
    chirps_tif <- paste0(in_basename, "_", dataset, '_', 
                         format(chirps_start_date, "%Y%m"), '-', 
                         format(chirps_end_date, "%Y%m"), '.tif')

    # Calculate the band numbers that are needed
    included_dates <- dates[(dates >= start_date) & (dates <= end_date)]
    band_nums <- c(1:length(dates))[(dates >= start_date) & (dates <= end_date)]

    chirps <- stack(chirps_tif, bands=band_nums)

    # Setup a dataframe with the precipitation data so anomalies, etc can be 
    # calculated
    years <- year(included_dates)
    years_rep <- rep(years, each=nrow(chirps)*ncol(chirps))
    subyears <- rep(seq(1, periods_per_year),  length.out=nlayers(chirps))
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
        mutate(tot_12mth=as.numeric(stats::filter(ppt, rep(1/12, 12), sides=1)*12))

    # Calculate the mean_monthly for each subyear for each pixel
    ppt_mean_monthly <- group_by(chirps_df, pixel, subyear) %>%
        summarize(mean_monthly=mean(ppt, na.rm=TRUE))
    # Use chirps raster as a template
    ppt_mean_monthly_rast <- brick(chirps, values=FALSE, nl=periods_per_year)
    ppt_mean_monthly_rast <- setValues(ppt_mean_monthly_rast,
                               matrix(ppt_mean_monthly$mean_monthly, 
                                      nrow=nrow(chirps)*ncol(chirps), 
                                      ncol=periods_per_year, byrow=TRUE))
    writeRaster(ppt_mean_monthly_rast,
                filename=paste0(out_basename, '_ppt_mean_monthly.tif'), 
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
                filename=paste0(out_basename, '_ppt_mean_12mth.tif'), 
                overwrite=TRUE)

    chirps_df$anom_12mth <- chirps_df$tot_12mth - ppt_mean_12mth$mean_annual[match(chirps_df$pixel, ppt_mean_12mth$pixel)]
    #filter(chirps_df, pixel == 1)[1:36,]

    save(chirps_df, file=paste0(out_basename, '_ppt.RData'))

    # Save rasters of 12 month anomalies
    anom_12mth_rast <- brick(chirps, values=FALSE, nl=nlayers(chirps))
    anom_12mth_rast <- setValues(anom_12mth_rast,
                                 matrix(chirps_df$anom_12mth, 
                                        nrow=nrow(chirps)*ncol(chirps), 
                                        ncol=nlayers(chirps)))
    anom_12mth_rast_filename <- paste0(out_basename, '_ppt_anom_12mth.tif')
    writeRaster(anom_12mth_rast, filename=anom_12mth_rast_filename, 
                overwrite=TRUE)

    return(TRUE)
}
