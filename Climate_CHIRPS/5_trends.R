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
included_subyears <- NA
#included_subyears <- c(10, 11, 12)

cl  <- makeCluster(3)
registerDoParallel(cl)

in_folder <- file.path(prefix, "GRP", "CHIRPS-2.0")
out_folder <- file.path(prefix, "GRP", "CHIRPS-2.0")
# in_folder <- file.path(prefix, "Vital_Signs", "CHIRPS-2.0")
# out_folder <- file.path(prefix, "Vital_Signs", "CHIRPS-2.0")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))

datafiles <- dir(in_folder, pattern='_CHIRPS_monthly_198101-201412.tif$')

datafiles <- datafiles[2:3]

foreach (datafile=datafiles) %do% {
    timestamp()
    name <- str_extract(datafile, '^[a-zA-Z]*')
    print(paste0("Processing ", name, "..."))

    # Calculate the band numbers that are needed
    dates <- seq(as.Date('1981/1/1'), as.Date('2014/12/1'), by='months')
    band_nums <- c(1:length(dates))[(dates >= start_date) & (dates <= end_date)]
    dates <- dates[band_nums]

    # Calculate season string
    if (!is.na(included_subyears)) {
        season_string <- paste0('_', paste(included_subyears, collapse='-'))
    } else {
        season_string <- ''
    }

    start_date_text <- format(start_date, '%Y%m%d')
    end_date_text <- format(end_date, '%Y%m%d')
    out_basename <- file.path(in_folder,
        paste0(gsub('[0-9-]*', '', file_path_sans_ext(datafile)),
              start_date_text, '-', end_date_text))

    chirps <- stack(file.path(in_folder, datafile), bands=band_nums)

    # Function to calculate trend
    calc_decadal_trend <- function(p, dates, included_subyears, ...) {
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
        if (!is.na(included_subyears)) {
            p_df <- dplyr::filter(p_df, subyear %in% included_subyears)
        }
        # Map areas that are getting signif. wetter or drier, coded by mm per 
        # year
        extract_coefs <- function(indata) {
            if (sum(!is.na(indata$ppt_annual_pctmean)) < 3) {
                d <- data.frame(coef=c('(Intercept)', 'year'), c(NA, NA), c(NA, NA))
            } else {
                model <- lm(ppt_annual_pctmean ~ year, data=indata)
                d <- data.frame(summary(model)$coefficients[, c(1, 4)])
                d <- cbind(row.names(d), d)
            }
            names(d) <- c('coef', 'estimate', 'p_val')
            row.names(d) <- NULL
            return(d)
        }
        lm_coefs <- group_by(p_df, year, pixel) %>%
            summarize(ppt_annual=sum(ppt, na.rm=TRUE)) %>%
            group_by(pixel) %>%
            mutate(ppt_annual_pctmean=(ppt_annual/mean(ppt_annual))*100) %>%
            do(extract_coefs(.))
        # Note the *10 below to convert to decadal change
        out <- array(c(filter(lm_coefs, coef == "year")$estimate * 10,
                       filter(lm_coefs, coef == "year")$p_val),
                     dim=c(dim(p)[1], dim(p)[2], 2))
        # Mask out nodata areas
        out[ , , 1][is.na(p[ , , 1])] <- NA
        out[ , , 2][is.na(p[ , , 1])] <- NA
        out
    }

    decadal_trend <- rasterEngine(p=chirps,
        args=list(dates=dates, included_subyears=included_subyears),
        fun=calc_decadal_trend, datatype='FLT4S', outbands=2, outfiles=1, 
        processing_unit="chunk",
        filename=paste0(out_basename, '_trend_decadal', season_string),
        .packages=c('dplyr', 'lubridate'))
    writeRaster(decadal_trend,
                filename=paste0(out_basename, '_trend_decadal', season_string, '_geotiff.tif'),
                overwrite=TRUE)

}

stopCluster(cl)
