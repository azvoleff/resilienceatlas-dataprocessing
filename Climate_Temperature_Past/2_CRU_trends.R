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

n_cpus <- 3

cl  <- makeCluster(n_cpus)
registerDoParallel(cl)

product <- 'cru_ts3.22'
datestring <- '1901.2013'

# Note the below code is INCLUSIVE of the start date
cru_start_date <- as.Date('1901/1/1')
# Note the below code is EXCLUSIVE of the end date
cru_end_date <- as.Date('2014/1/1')

yrs <- seq(year(cru_start_date), year(cru_end_date))
dates <- seq(cru_start_date, cru_end_date, by='months')
dates <- dates[dates < cru_end_date]
num_periods <- 12

# Choose a start and end year for the data to include in this analysis
start_date <- as.Date('1984/1/1') # Inclusive
end_date <- as.Date('2014/1/1') # Exclusive

datasets <- c('tmn', 'tmx', 'tmp', 'pet')

in_folder <- file.path(prefix, "GRP", "CRU")
out_folder <- file.path(prefix, "GRP", "CRU")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))

iso_key <- read.csv(file.path('..', "ISO_Codes.csv"))

s_srs <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'

ISO_2s <- c("ID", "UG", "NE", "ET", "ER")

foreach (dataset=datasets, .inorder=FALSE,
         .packages=c("rgdal", "lubridate", "dplyr", "raster")) %:% {
    foreach (ISO_2=ISO_2s, .inorder=FALSE) %dopar% {
        filename_base <- paste0(ISO_2, '_', product, '_', dataset, '_')
        cru_data_file <- file.path(out_folder,
                              paste0(filename_base, datestring,  '.tif'))

        # Calculate the band numbers that are needed
        included_dates <- dates[(dates >= start_date) & (dates < end_date)]
        band_nums <- c(1:length(dates))[(dates >= start_date) & (dates < end_date)]

        cru_data <- stack(cru_data_file, bands=band_nums)

        # Setup a dataframe with the precipitation data so anomalies, etc. can 
        # be calculated
        years <- year(included_dates)
        years_rep <- rep(years, each=nrow(cru_data)*ncol(cru_data))
        subyears <- rep(seq(1, num_periods),  length.out=nlayers(cru_data))
        subyears_rep <- rep(subyears, each=nrow(cru_data)*ncol(cru_data))
        pixels_rep <- rep(seq(1:(nrow(cru_data)*ncol(cru_data))), nlayers(cru_data))
        cru_data_df <- data.frame(year=years_rep,
                                  subyear=subyears_rep, 
                                  pixel=pixels_rep,
                                  cru_data=as.vector(cru_data))
        cru_data_df <- tbl_df(cru_data_df)

        # Map areas that are getting signif. wetter or drier, coded by mm per year
        extract_coefs <- function(model) {
            d <- data.frame(summary(model)$coefficients[, c(1, 4)])
            d <- cbind(row.names(d), d)
            names(d) <- c('coef', 'estimate', 'p_val')
            row.names(d) <- NULL
            return(d)
        }
        annual_lm_coefs <- group_by(cru_data_df, year, pixel) %>%
            summarize(annual_mean=mean(cru_data, na.rm=TRUE)) %>%
            group_by(pixel) %>%
            do(extract_coefs(lm(annual ~ year, data=.)))

        # Use cru_data raster as a template
        annual_slope_rast <- brick(cru_data, values=FALSE, nl=1)
        annual_slope_rast <- setValues(annual_slope_rast,
                                   matrix(filter(annual_lm_coefs, coef == "year")$estimate, 
                                          nrow=nrow(cru_data)*ncol(cru_data), 
                                          ncol=1, byrow=TRUE))
        annual_slope_rast <- writeRaster(annual_slope_rast,
                    filename=file.path(out_folder, paste0(filename_base, 
                                                          'annual_slope.tif')), 
                    overwrite=TRUE)

        annual_p_val_rast <- brick(cru_data, values=FALSE, nl=1)
        annual_p_val_rast <- setValues(annual_p_val_rast,
                                   matrix(filter(annual_lm_coefs, coef == "year")$p_val, 
                                          nrow=nrow(cru_data)*ncol(cru_data), 
                                          ncol=1, byrow=TRUE))
        annual_p_val_rast <- writeRaster(annual_p_val_rast,
                    filename=file.path(out_folder, paste0(filename_base, 
                                                          'annual_pval.tif')), 
                    overwrite=TRUE)

        annual_slope_rast <- overlay(annual_slope_rast, annual_p_val_rast,
            fun=function(slp, p) {
                return(slp * (p < .05))
            },
            filename=file.path(out_folder, paste0(filename_base, 
                                                  'annual_slope_masked.tif')),
            overwrite=TRUE)
    }
}

stopCluster(cl)
