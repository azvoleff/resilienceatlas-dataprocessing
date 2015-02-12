###############################################################################
# Extracts precipitation timeseries for specific points.
###############################################################################

source('../0_settings.R')

library(raster)
library(stringr)
library(reshape2)
library(rgdal)
library(lubridate)

library(doParallel)
library(foreach)

n_cpus <- 4

overwrite <- TRUE

product <- 'v1p8chirps'
chirps_NA_value <- -9999
dataset <- 'monthly' # For SPI, use monthly
date_limits_string <- '198101-201404'

# Select the start and end dates for the data to include in this analysis
start_date <- as.Date('1985/1/1') # Inclusive
end_date <- as.Date('2014/12/1') # Exclusive

# Note the below code is INCLUSIVE of the start date
yrs <- seq(year(start_date), year(end_date))
dates <- seq(start_date, end_date, by='months')
periods_per_year <- 12

in_folder <- file.path(prefix, "GRP", "CHIRPS")
out_folder <- file.path(prefix, "GRP", "CHIRPS")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))
shp_folder <- file.path(prefix, "GRP", "Boundaries")


aoi_polygons <- readOGR(shp_folder, 'Analysis_Areas')
aoi_polygons <- aoi_polygons[aoi_polygons$Type == "Country", ]

spi_period <- 12

for (n in 1:nrow(aoi_polygons)) {
    timestamp()
    aoi <- aoi_polygons[n, ]
    name <- as.character(aoi$Name)
    name <- gsub(' ', '', name)

    file_basename <- file.path(out_folder,
                             paste0(name, '_CHIRPS_',
                                    format(start_date, "%Y%m"), '-',
                                    format(end_date, "%Y%m")))

    spi_tif <- paste0(file_basename, '_SPI_', spi_period, '.tif')

    # Calculate the band numbers that are needed
    spi <- stack(spi_tif)

    # Now extract timeseries averaged over the aoi
    aoi <- spTransform(aoi, CRS(proj4string(spi)))

    spi <- mask(spi, aoi)

    # Remember spi was scaled by 1000 to save as an INT2S
    spi_ts <- data.frame(date=dates, spi=cellStats(spi, "mean")/1000, 
                         period=spi_period)

    save(spi_ts, file=paste0(file_basename, "_SPI_", spi_period, 
                             "_meantimeseries.RData"))

}
