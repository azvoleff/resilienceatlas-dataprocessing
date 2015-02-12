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

n_cpus <- 15

cl  <- makeCluster(n_cpus)
registerDoParallel(cl)

in_folder <- file.path(prefix, "GRP", "CHIRPS")
out_folder <- file.path(prefix, "GRP", "CHIRPS")
shp_folder <- file.path(prefix, "GRP", "Boundaries")

dataset <- 'monthly' # For SPI, use monthly

# Note the below code is INCLUSIVE of the start date
chirps_start_date <- as.Date('1981/1/1')
# Note the below code is INCLUSIVE of the end date
chirps_end_date <- as.Date('2014/12/1')
yrs <- seq(year(chirps_start_date), year(chirps_end_date))
dates <- seq(chirps_start_date, chirps_end_date, by='months')

# Select the start and end dates for the data to include in this analysis
start_date <- as.Date('1985/1/1') # Inclusive
end_date <- as.Date('2014/12/1') # Exclusive

stopifnot(dataset == "monthly")

aoi_polygons <- readOGR(shp_folder, 'Analysis_Areas')
aoi_polygons <- aoi_polygons[aoi_polygons$Type == "Country", ]

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

for (n in 1:nrow(aoi_polygons)) {
    timestamp()
    aoi <- aoi_polygons[n, ]
    name <- as.character(aoi$Name)
    name <- gsub(' ', '', name)

    print(paste0("Processing ", name, "..."))

    in_basename <- file.path(out_folder,
                           paste0(name, '_CHIRPS_', dataset,
                                  '_', format(chirps_start_date, "%Y%m"), '-', 
                                  format(chirps_end_date, "%Y%m")))

    chirps_tif <- paste0(in_basename, '.tif')

    out_basename <- file.path(out_folder,
                              paste0(name, '_CHIRPS_',
                                     format(start_date, "%Y%m"), '-',
                                     format(end_date, "%Y%m")))

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
