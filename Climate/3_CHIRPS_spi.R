###############################################################################
# Calculates SPI for GRP countries.
###############################################################################

source('../0_settings.R')

library(raster)
library(SPEI)
library(foreach)
library(doParallel)

n_cpus <- 25

ISO_2s <- c("ET", "DJ", "SO", "ER")

cl  <- makeCluster(n_cpus)
registerDoParallel(cl)

in_folder <- file.path(prefix, "GRP", "CHIRPS")
out_folder <- file.path(prefix, "GRP", "CHIRPS")

product <- 'v1p8chirps'

dataset <- 'monthly' # For SPI, use monthly
date_limits_string <- '198101-201404'

stopifnot(dataset == "monthly")

spi_lengths <- c(24, 36)

# Define function to calculate SPI
calc_spi <- function(chirps_mat, spi_period, filename_base) {
    # Split the chirps_mat into pieces to minimize the number of calls to the
    # spi function
    start_n <- floor(seq(1, ncol(chirps_mat),
                         length.out=min(ncol(chirps_mat), n_cpus)))
    end_n <- start_n[2:length(start_n)]
    start_n <- start_n[1:(length(start_n) - 1)]
    end_n <- end_n - 1
    end_n[length(end_n)] <- end_n[length(end_n)] + 1

    spi_mat <- foreach(start_n=start_n, end_n=end_n,
                       .combine=cbind, .packages=c("SPEI")) %dopar% {
        spi(chirps_mat[, start_n:end_n], spi_period, na.rm=TRUE)$fitted
    }

    spi_rast <- brick(chirps, values=FALSE, nl=nlayers(chirps))
    spi_rast <- setValues(spi_rast, t(spi_mat))
    spi_filename <- file.path(out_folder,
                              paste0(filename_base, 'SPI_', 
                                     spi_period, '.tif'))
    writeRaster(spi_rast, spi_filename, overwrite=TRUE)
}

for (ISO_2 in ISO_2s) {
    timestamp()
    message('Processing ', ISO_2, '...')

    filename_base <- paste0(ISO_2, '_', product, '_', dataset, '_')

    chirps_tif_masked <- file.path(out_folder,
                            paste0(filename_base, date_limits_string, 
                                   '_NAs_masked.tif'))

    chirps_file_masked <- file.path(in_folder,
                          paste0(product, '_', dataset, '_VitalSigns_', 
                                 date_limits_string, '_NAsmasked.tif'))
    chirps <- brick(chirps_file_masked)
    chirps_layers_in_cols <- t(as.matrix(chirps))

    for (spi_period in spi_periods) {
        calc_spi(chirps_layers_in_cols, spi_period, filename_base)
    }
}

stopCluster(cl)
