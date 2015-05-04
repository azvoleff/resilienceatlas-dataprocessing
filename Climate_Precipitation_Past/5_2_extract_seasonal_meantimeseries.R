source('../0_settings.R')

library(rgdal)
library(raster)
library(lubridate)
library(dplyr)
library(rgeos)
library(foreach)
library(doParallel)
library(RColorBrewer)
library(ggplot2)
library(gridExtra)
library(broom)

cl  <- makeCluster(3)
registerDoParallel(cl)

# For monthly data:
dataset <- 'monthly' # For SPI, use monthly

in_folder <- file.path(prefix, "GRP", "CHIRPS-2.0")
out_folder <- file.path(prefix, "GRP", "CHIRPS-2.0")
shp_folder <- file.path(prefix, "GRP", "Boundaries")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))
stopifnot(file_test('-d', shp_folder))

# Note the below code is INCLUSIVE of the start date
chirps_start_date <- as.Date('1981/1/1')
# Note the below code is INCLUSIVE of the end date
chirps_end_date <- as.Date('2014/12/1')
yrs <- seq(year(chirps_start_date), year(chirps_end_date))
dates <- seq(chirps_start_date, chirps_end_date, by='months')
periods_per_year <- 12

# Select the start and end dates for the data to include in this analysis
start_date <- as.Date('1981/1/1') # Inclusive
end_date <- as.Date('2014/12/1') # Exclusive

aoi_polygons <- readOGR(shp_folder, 'Analysis_Areas')
aoi_polygons <- aoi_polygons[aoi_polygons$Name == "Horn of Africa", ]

foreach (n=1:nrow(aoi_polygons), .inorder=FALSE,
         .packages=c("rgdal", "lubridate", "dplyr", "raster",
                     "rgeos")) %dopar% {
    timestamp()
    aoi <- aoi_polygons[n, ]
    name <- as.character(aoi$Name)
    name <- gsub(' ', '', name)

    print(paste0("Processing ", name, "..."))

    in_basename <- file.path(in_folder, paste0(name, '_CHIRPS'))

    out_basename <- file.path(out_folder, paste0(name, '_CHIRPS_',
                                format(start_date, "%Y%m"), '-', 
                                format(end_date, "%Y%m")))
                                  
    chirps_tif <- paste0(in_basename, "_", dataset, '_', 
                         format(chirps_start_date, "%Y%m"), '-', 
                         format(chirps_end_date, "%Y%m"), '.tif')

    # Calculate the band numbers that are needed
    included_dates <- dates[(dates >= start_date) & (dates <= end_date)]
    band_nums <- c(1:length(dates))[(dates >= start_date) & (dates <= end_date)]

    chirps <- stack(chirps_tif, bands=band_nums)

    # Setup a dataframe with the precipitation data so anomalies, etc. can be 
    # calculated
    years <- year(included_dates)
    n_years <- length(unique(years))
    years_rep <- rep(years, each=nrow(chirps)*ncol(chirps))
    subyears <- rep(seq(1, periods_per_year),  length.out=nlayers(chirps))
    subyears_rep <- rep(subyears, each=nrow(chirps)*ncol(chirps))
    pixels_rep <- rep(seq(1:(nrow(chirps)*ncol(chirps))), nlayers(chirps))
    chirps_df <- data.frame(year=years_rep,
                            subyear=subyears_rep, 
                            pixel=pixels_rep,
                            ppt=as.vector(chirps))
    chirps_df <- tbl_df(chirps_df)
    chirps_df$ppt[chirps_df$ppt < 0] <- NA

    chirps_df$season <- NA
    chirps_df$season[chirps_df$subyear %in% c(10, 11, 12, 1)] <- 'Dry'
    chirps_df$season[chirps_df$subyear %in% c(2, 3, 4, 5)] <- 'Short rains'
    chirps_df$season[chirps_df$subyear %in% c(6, 7, 8, 9)] <- 'Long rains'

    # Count January rains with the prior year's season
    chirps_df$year[chirps_df$subyear == 1] <- chirps_df$year[chirps_df$subyear == 1] - 1
    # So drop the first january since it applies to 1980 season
    chirps_df <- filter(chirps_df, year >= 1981)

    seasonal_ppt <- group_by(chirps_df, season, year, pixel) %>%
        summarise(total=sum(ppt, na.rm=TRUE))

    # # Calculate surface with mean seasonal precipitation totals
    # seasonal_ppt_mean <- group_by(seasonal_ppt, season, pixel) %>%
    #     summarise(total=mean(total, na.rm=TRUE)) %>%
    #     arrange(season, pixel)

    # Plot timeseries of mean seasonal ppt
    seasonal_ppt_mean_ts <- group_by(seasonal_ppt, season, year) %>%
        summarise(total=mean(total, na.rm=TRUE)) %>%
        group_by(season) %>%
        mutate(anomaly=total - mean(total))

    ggplot(seasonal_ppt_mean_ts, aes(year, total, colour=season)) + 
        theme_bw(base_size=10) +
        geom_line() +
        scale_colour_brewer("Season", palette='Set2') +
        xlab('Year') +
        ylab('Precipitation (mm)') +
        theme(legend.position=c(.15, .8),
              legend.key.size=unit(1, "cm"),
              legend.key.height=unit(.5, "cm"))
    ggsave(paste0(out_basename, '_seasonal_timeseries.png'), dpi=300,
           width=8, height=6)

    ggplot(seasonal_ppt_ts, aes(year, anomaly, colour=season)) + 
        theme_bw(base_size=10) +
        geom_line() +
        scale_colour_brewer("Season", palette='Set2') +
        xlab('Year') +
        ylab('Precipitation anomaly (mm)') +
        theme(legend.position=c(.15, .8),
              legend.key.size=unit(1, "cm"),
              legend.key.height=unit(.5, "cm"))
    ggsave(paste0(out_basename, '_seasonal_timeseries_anomaly.png'), dpi=300,
           width=8, height=6)

        ## TODO: This doesn't extract significance due to bug in R - update to use 
    ## broom when this bug is fixed
    # Map areas that are getting signif. wetter or drier, coded by mm per year
    extract_coefs <- function(model) {
        d <- coef(model)
        d <- data.frame(matrix(d, ncol=length(d)))
        names(d) <- names(coef(model))
        d
    }

    foreach (this_season=unique(seasonal_ppt$season),
             .packages=c('broom', 'dplyr', 'raster', 'rgdal',
                         'ggplot2')) %dopar% {
        this_seasonal_ppt <- filter(seasonal_ppt, season == this_season) %>%
            select(-season)
        #TODO: Get this working from http://bit.ly/1Jl4aaI
        # seasonal_pixel_fits <- group_by(this_seasonal_ppt, pixel) %>%
        #     do(fit=lm(total ~ year, data=.))
        # seasonal_fits <- tidy(seasonal_pixel_fits, fit)

        this_basename <- paste0(out_basename, '_', paste(this_season, collapse=''))
        season_total_rast <- brick(chirps, values=FALSE, nl=n_years)
        season_total_rast <- setValues(season_total_rast,
                                       matrix(this_seasonal_ppt$total, 
                                              nrow=nrow(chirps)*ncol(chirps), 
                                              ncol=n_years))
        writeRaster(season_total_rast,
                    filename=paste0(this_basename, '_total.tif'), 
                    overwrite=TRUE)
        
        # Add a fraction of mean total column
        this_seasonal_ppt <- group_by(this_seasonal_ppt, pixel) %>%
            mutate(total_frac=total/mean(total, na.rm=TRUE))
        # Handle NAs induced by division of areas with zero mean ppt (oceans)
        this_seasonal_ppt$total_frac[is.na(this_seasonal_ppt$total_frac)] <- 0
        # Model change in precipitation as fraction of pixel-level mean total
        seasonal_lm_coefs <- group_by(this_seasonal_ppt, pixel) %>%
            do(extract_coefs(lm(total_frac ~ year, data=.)))

        # Note that the units are fraction of pixel-level mean per decade - 
        # hence the multiplication by 10 below
        slope_rast <- brick(chirps, values=FALSE, nl=1)
        slope_rast <- setValues(slope_rast,
                                matrix(seasonal_lm_coefs$year * 10, 
                                       nrow=nrow(chirps)*ncol(chirps), 
                                       ncol=1, byrow=TRUE))
        slope_rast <- writeRaster(slope_rast,
                                  filename=paste0(this_basename, '_decadalslope.tif'), 
                                  overwrite=TRUE)

        # p_val_rast <- brick(chirps, values=FALSE, nl=1)
        # p_val_rast <- setValues(p_val_rast,
        #                         matrix(filter(seasonal_lm_coefs, coef == "year")$p_val, 
        #                                nrow=nrow(chirps)*ncol(chirps), 
        #                                ncol=1, byrow=TRUE))
        # p_val_rast <- writeRaster(p_val_rast,
        #                           filename=paste0(this_basename, '_pval.tif'), 
        #                           overwrite=TRUE)
        #
        # slope_rast <- overlay(slope_rast, p_val_rast,
        #     fun=function(slp, p) {
        #         return(slp * (p < .05))
        #     },
        #     filename=paste0(out_basename, '_slope_masked.tif'),
        #     overwrite=TRUE)
    }
    
}

stopCluster(cl)
