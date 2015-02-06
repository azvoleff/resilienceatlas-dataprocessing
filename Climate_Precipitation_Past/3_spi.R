###############################################################################
# Calculates SPI for GRP countries.
###############################################################################

source('../0_settings.R')

library(raster)
library(SPEI)
library(foreach)
library(doParallel)

n_cpus <- 20

cl  <- makeCluster(n_cpus)
registerDoParallel(cl)

in_folder <- file.path(prefix, "GRP", "CHIRPS")
out_folder <- file.path(prefix, "GRP", "CHIRPS")

product <- 'v1p8chirps'

dataset <- 'monthly' # For SPI, use monthly

# Note the below code is INCLUSIVE of the start date
chirps_start_date <- as.Date('1981/1/1')
# Note the below code is INCLUSIVE of the end date
chirps_end_date <- as.Date('2014/12/1')
yrs <- seq(year(chirps_start_date), year(chirps_end_date))
dates <- seq(chirps_start_date, chirps_end_date, by='months')
periods_per_year <- 12

# Select the start and end dates for the data to include in this analysis
start_date <- as.Date('1985/1/1') # Inclusive
end_date <- as.Date('2014/12/1') # Exclusive

stopifnot(dataset == "monthly")

region_polygons <- readOGR(shp_folder, 'GRP_regions')

region_rows <- c(2, 3, 5)

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

for (n in region_rows) {
    timestamp()
    timestamp()
    aoi <- region_polygons[n, ]
    region <- as.character(aoi$Region)
    region <- gsub(' ', '', region)

    print(paste0("Processing ", region, "..."))

    base_name <- file.path(out_folder,
                           paste0(region, '_CHIRPS_', dataset,
                                  '_', format(chirps_start_date, "%Y%m"), '-', 
                                  format(chirps_end_date, "%Y%m")))

    chirps_tif_masked <- paste0(base_name, '_NAs_masked.tif')

    # Calculate the band numbers that are needed
    included_dates <- dates[(dates >= start_date) & (dates <= end_date)]
    band_nums <- c(1:length(dates))[(dates >= start_date) & (dates <= end_date)]

    chirps <- stack(chirps_tif_masked, bands=band_nums)

    chirps_mat <- t(as.matrix(chirps))

    for (spi_period in spi_periods) {
        spi_mat <- calc_spi(chirps_mat, spi_period)
        out_rast <- brick(chirps, values=FALSE, nl=nlayers(chirps))
        out_rast <- setValues(out_rast, t(spi_mat))
        spi_filename <- file.path(out_folder,
                                  paste0(filename_base, 'SPI_', 
                                         spi_period, '.tif'))
        out_rast <- writeRaster(out_rast, spi_filename, overwrite=TRUE,
                                datatype="INT2S")
    }
}

stopCluster(cl)
