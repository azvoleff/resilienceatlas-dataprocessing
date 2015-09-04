###############################################################################
# Calculates mean precipitation for GRP countries.
###############################################################################

source('../0_settings.R')

library(tools)
library(stringr)
library(raster)
library(lubridate)
library(dplyr)
library(rgeos)
library(teamlucc)
library(foreach)
library(doParallel)
library(spatial.tools)

# Select the start and end dates for the data to include in this analysis
start_date <- as.Date('1985/1/1') # Inclusive
end_date <- as.Date('2014/12/1') # Exclusive

cl  <- makeCluster(3)
registerDoParallel(cl)

# in_folder <- file.path(prefix, "GRP", "CHIRPS-2.0")
# out_folder <- file.path(prefix, "GRP", "CHIRPS-2.0")
in_folder <- file.path(prefix, "Vital_Signs", "CHIRPS-2.0")
out_folder <- file.path(prefix, "Vital_Signs", "CHIRPS-2.0")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))

datafiles <- dir(in_folder, pattern='_CHIRPS_monthly_198101-201412.tif$')
datafile <- datafiles[1]

foreach (datafile=datafiles) %do% {
    timestamp()
    name <- str_extract(datafile, '^[a-zA-Z]*')
    print(paste0("Processing ", name, "..."))

    start_date_text <- format(start_date, '%Y%m%d')
    end_date_text <- format(end_date, '%Y%m%d')
    out_basename <- file.path(in_folder,
        paste0(gsub('[0-9-]*', '', file_path_sans_ext(datafile)),
              start_date_text, '-', end_date_text))

    # Calculate the band numbers that are needed
    dates <- seq(as.Date('1981/1/1'), as.Date('2014/12/1'), by='months')
    dates <- dates[(dates >= start_date) & (dates <= end_date)]
    band_nums <- c(1:length(dates))[(dates >= start_date) & (dates <= end_date)]

    ### TEMPORARY
    band_nums <- 49:84
    ### TEMPORARY

    chirps <- stack(file.path(in_folder, datafile), bands=band_nums)

    ### TODO: Need to be producing outputs in units of percent change relative 
    ### to baseline total annual precipitation

    # Function to calculate trend
    calc_trend <- function(p, dates, ...) {
        p[p == -9999] <- NA
        # Setup period identifiers so the data can be used in a dataframe
        years <- year(dates)
        years_rep <- rep(years, each=dim(p)[1]*dim(p)[2])
        subyears <- rep(seq(1, 12),  length.out=dim(p)[3])
        subyears_rep <- rep(subyears, each=dim(p)[1]*dim(p)[2])
        pixels_rep <- rep(seq(1:(dim(p)[1]*dim(p)[2])), dim(p)[3])
        p_df <- data.frame(year=years_rep,
                           subyear=subyears_rep, 
                           pixel=pixels_rep,
                           ppt=as.vector(p))
        # Map areas that are getting signif. wetter or drier, coded by mm per 
        # year
        extract_coefs <- function(model) {
            d <- data.frame(summary(model)$coefficients[, c(1, 4)])
            d <- cbind(row.names(d), d)
            names(d) <- c('coef', 'estimate', 'p_val')
            row.names(d) <- NULL
            return(d)
        }
        lm_coefs <- group_by(p_df, year, pixel) %>%
            summarize(ppt_annual=sum(ppt, na.rm=TRUE)) %>%
            group_by(pixel) %>%
            mutate(ppt_annual_pctmean=(ppt_annual/sum(ppt_annual))*100) %>%
            do(extract_coefs(lm(ppt_annual_pctmean ~ year, data=.)))
        out <- array(c(filter(lm_coefs, coef == "year")$estimate,
                       filter(lm_coefs, coef == "year")$p_val),
                     dim=c(dim(p)[1], dim(p)[2], 2))
        # Mask out nodata areas
        out[, , 1][is.na(p[ , , 1])] <- NA
        out[, , 2][is.na(p[ , , 1])] <- NA
        out
    }
    annual_trend <- rasterEngine(p=chirps, args=list(dates=dates),
        fun=calc_trend, datatype='FLT4S', outbands=2, outfiles=1, 
        processing_unit="chunk",
        filename=paste0(out_basename, '_trend_annual'),
        .packages=c('dplyr', 'lubridate'))

}

stopCluster(cl)
