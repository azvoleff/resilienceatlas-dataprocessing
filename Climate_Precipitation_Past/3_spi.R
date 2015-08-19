###############################################################################
# Calculates SPI for GRP countries.
###############################################################################

source('../0_settings.R')

library(rgdal)
library(raster)
library(SPEI)
library(foreach)
library(doParallel)
library(lubridate)

spi_periods <- c(12)

cl  <- makeCluster(12)
registerDoParallel(cl)

# Select the start and end dates for the data to include in this analysis
start_date <- as.Date('1985/1/1') # Inclusive
end_date <- as.Date('2014/12/1') # Exclusive

in_folder <- file.path(prefix, "GRP", "CHIRPS-2.0")
out_folder <- file.path(prefix, "GRP", "CHIRPS-2.0")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))

# Define function to calculate SPI
calc_spi <- function(chirps_mat, spi_period) {
    # Split the chirps_mat into pieces to minimize the number of calls to the
    # spi function
    start_n <- floor(seq(1, ncol(chirps_mat),
                         length.out=min(ncol(chirps_mat), n_cpus + 1)))
    end_n <- start_n[2:length(start_n)]
    start_n <- start_n[1:(length(start_n) - 1)]
    end_n <- end_n - 1
    end_n[length(end_n)] <- end_n[length(end_n)] + 1
    spi_mat <- foreach(start_n=start_n, end_n=end_n,
                       .combine=cbind, .packages=c("SPEI")) %dopar% {
        # Multiply by 1000 and round so results can be stored as INT2S
        round(spi(chirps_mat[, start_n:end_n], spi_period, na.rm=TRUE)$fitted * 1000)
    }
    return(spi_mat)
}

datafiles <- dir(in_folder, pattern='_CHIRPS_monthly_198101-201412.tif$')
foreach (datafile=datafiles) %do% {
    timestamp()
    name <- str_extract(datafile, '^[a-zA-Z]*')
    print(paste0("Processing ", name, "..."))
    out_basename <- file.path(in_folder, file_path_sans_ext(datafile))

    # Calculate the band numbers that are needed
    dates <- seq(as.Date('1981/1/1'), as.Date('2014/12/1'), by='months')
    dates <- dates[(dates >= start_date) & (dates <= end_date)]
    band_nums <- c(1:length(dates))[(dates >= start_date) & (dates <= end_date)]

    ### TEMPORARY
    band_nums <- 49:84
    ### TEMPORARY

    chirps <- stack(file.path(in_folder, datafile), bands=band_nums)

    out_basename <- file.path(in_folder,
        paste0(gsub('[0-9-]*', '', file_path_sans_ext(datafile)),
              start_date_text, '-', end_date_text))

    # Calculate the band numbers that are needed
    included_dates <- dates[(dates >= start_date) & (dates <= end_date)]
    band_nums <- c(1:length(dates))[(dates >= start_date) & (dates <= end_date)]

    chirps <- stack(chirps_tif, bands=band_nums)

    chirps_mat <- t(as.matrix(chirps))

    for (spi_period in spi_periods) {
        spi_mat <- calc_spi(chirps_mat, spi_period)
        out_rast <- brick(chirps, values=FALSE, nl=nlayers(chirps))
        out_rast <- setValues(out_rast, t(spi_mat))
        spi_filename <- paste0(out_basename, '_SPI_', spi_period, '.tif')
        out_rast <- writeRaster(out_rast, spi_filename, overwrite=TRUE,
                                datatype="INT2S")
    }
}

stopCluster(cl)
