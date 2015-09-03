###############################################################################
# Calculates SPI for GRP countries.
###############################################################################

source('../0_settings.R')

library(rgdal)
library(stringr)
library(raster)
library(SPEI)
library(foreach)
library(doParallel)
library(lubridate)
library(tools)
library(spatial.tools)

spi_periods <- c(12)

cl <- makeCluster(20)
registerDoParallel(cl)

# Select the start and end dates for the data to include in this analysis
start_date <- as.Date('1981/1/1') # Inclusive
end_date <- as.Date('2014/12/1') # Exclusive

# in_folder <- file.path(prefix, "GRP", "CHIRPS-2.0")
# out_folder <- file.path(prefix, "GRP", "CHIRPS-2.0")
in_folder <- file.path(prefix, "Vital_Signs", "CHIRPS-2.0")
out_folder <- file.path(prefix, "Vital_Signs", "CHIRPS-2.0")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))

# Define function to calculate SPI
calc_spi <- function(p, spi_period, ...) {
    browser()
    p[p == -9999] <- NA
    dim_p <- dim(p)
    # SPI expects each timeseries in its own column. So first store the pixels 
    # in rows:
    p <- t(array(p, dim=c(dim_p[1] * dim_p[2], dim_p[3])))
    # Multiply by 1000 and round so results can be stored as INT2S
    out <- round(spi(p, spi_period, na.rm=TRUE)$fitted * 1000)
    # Convert back to having timeseries in the z-direction
    out <- array(t(out), dim=dim_p)
    # rasterEngine does not properly handle NAs, so recode these to -32767
    out[is.na(out)] <- -32767
    out
}

datafiles <- dir(in_folder, pattern='_CHIRPS_monthly_198101-201412.tif$')

foreach (datafile=datafiles) %do% {
    timestamp()
    name <- str_extract(datafile, '^[a-zA-Z]*')
    print(paste0("Processing ", name, "..."))
    out_basename <- file.path(in_folder, file_path_sans_ext(datafile))

    # Calculate the band numbers that are needed
    dates <- seq(as.Date('1981/1/1'), as.Date('2014/12/1'), by='months')
    band_nums <- c(1:length(dates))[(dates >= start_date) & (dates <= end_date)]

    start_date_text <- format(start_date, '%Y%m%d')
    end_date_text <- format(end_date, '%Y%m%d')
    out_basename <- file.path(in_folder,
        paste0(gsub('[0-9-]*', '', file_path_sans_ext(datafile)),
              start_date_text, '-', end_date_text))

    chirps <- stack(file.path(in_folder, datafile), bands=band_nums)

    for (spi_period in spi_periods) {
        spi_out <- rasterEngine(p=chirps, args=list(spi_period=spi_period),
            fun=calc_spi, outbands=nlayers(chirps), datatype='INT2S', 
            processing_unit="chunk", outfiles=1, .packages=c('SPEI'),
            filename=paste0(out_basename, '_SPI_', spi_period, 'mth'))
    }
}

stopCluster(cl)
