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
library(spatial.tools)

n_cpus <- 4

cl  <- makeCluster(n_cpus)
registerDoParallel(cl)

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

iso_key <- read.csv(file.path('..', "ISO_Codes.csv"))

yrs <- seq(year(chirps_start_date), year(chirps_end_date))
dates <- seq(chirps_start_date, chirps_end_date, by='months')
dates <- dates[dates < chirps_end_date]
num_periods <- 12

# This is the projection of the CHIRPS files, read from the .hdr files 
# accompanying the data
s_srs <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'

ISO_2s <- c("ID", "UG", "NE", "ET", "ER")

foreach (ISO_2=ISO_2s,
         .packages=c("rgdal", "lubridate", "dplyr", "raster")) %do% {
    timestamp()
    message('Processing ', ISO_2, '...')

    filename_base <- paste0(ISO_2, '_', dataset, '_')
    chirps_tif_masked <- file.path(out_folder,
                            paste0(filename_base, date_limits_string, 
                                   '_NAs_masked.tif'))
    chirps <- brick(chirps_tif_masked)

    # Setup a dataframe with the precipitation data so anomalies, etc. can be 
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

    # Map areas that are getting signif. wetter or drier, coded by mm per year
    extract_coefs <- function(model) {
        d <- data.frame(summary(model)$coefficients[, c(1, 4)])
        d <- cbind(row.names(d), d)
        names(d) <- c('coef', 'estimate', 'p_val')
        row.names(d) <- NULL
        return(d)
    }
    annual_ppt_lm_coefs <- group_by(chirps_df, year, pixel) %>%
        summarize(ppt_annual=sum(ppt, na.rm=TRUE)) %>%
        group_by(pixel) %>%
        do(extract_coefs(lm(ppt_annual ~ year, data=.)))

    # Use chirps raster as a template
    annual_ppt_slope_rast <- brick(chirps, values=FALSE, nl=1)
    annual_ppt_slope_rast <- setValues(annual_ppt_slope_rast,
                               matrix(filter(annual_ppt_lm_coefs, coef == "year")$estimate, 
                                      nrow=nrow(chirps)*ncol(chirps), 
                                      ncol=1, byrow=TRUE))
    annual_ppt_slope_rast <- writeRaster(annual_ppt_slope_rast,
                filename=file.path(out_folder, paste0(filename_base, 
                                                      'annual_ppt_slope.tif')), 
                overwrite=TRUE)

    annual_ppt_p_val_rast <- brick(chirps, values=FALSE, nl=1)
    annual_ppt_p_val_rast <- setValues(annual_ppt_p_val_rast,
                               matrix(filter(annual_ppt_lm_coefs, coef == "year")$p_val, 
                                      nrow=nrow(chirps)*ncol(chirps), 
                                      ncol=1, byrow=TRUE))
    annual_ppt_p_val_rast <- writeRaster(annual_ppt_p_val_rast,
                filename=file.path(out_folder, paste0(filename_base, 
                                                      'annual_ppt_pval.tif')), 
                overwrite=TRUE)

    annual_ppt_slope_rast <- overlay(annual_ppt_slope_rast, annual_ppt_p_val_rast,
        fun=function(slp, p) {
            return(slp * (p < .05))
        },
        filename=file.path(out_folder, paste0(filename_base, 
                                              'annual_ppt_slope_masked.tif')),
        overwrite=TRUE)
}
