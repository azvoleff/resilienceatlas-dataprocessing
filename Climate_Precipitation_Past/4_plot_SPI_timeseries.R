###############################################################################
# Extracts precipitation timeseries for specific points.
###############################################################################

source('../0_settings.R')

library(raster)
library(stringr)
library(reshape2)
library(rgdal)
library(lubridate)
library(ggplot2)
library(dplyr)
library(scales)

library(doParallel)
library(foreach)

cl  <- makeCluster(3)
registerDoParallel(cl)

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

foreach (n=1:nrow(aoi_polygons), .inorder=FALSE,
         .packages=c('raster','rgdal', 'ggplot2', 'dplyr', 'scales')) %dopar% {
    timestamp()
    aoi <- aoi_polygons[n, ]
    name <- as.character(aoi$Name)
    name <- gsub(' ', '', name)

    file_basename <- file.path(out_folder,
                             paste0(name, '_CHIRPS_',
                                    format(start_date, "%Y%m"), '-',
                                    format(end_date, "%Y%m")))

    load(paste0(file_basename, "_SPI_", spi_period, "_meantimeseries.RData"))

    # Filter to only include valid periods
    names(spi_ts)[names(spi_ts) == "date"] <- "obsdate"
    spi_ts <- filter(spi_ts, obsdate >= as.Date('1986/1/1'), obsdate <= as.Date('2012/1/1'))
    p1 <- ggplot(spi_ts) +
        theme_bw() +
        geom_line(aes(obsdate, spi), colour="blue") +
        xlab('Year') + ylab(expression('Std. Precip. Index')) +
        scale_x_date(breaks=c(as.Date('1990/1/1'), as.Date('2000/1/1'),
                              as.Date('2010/1/1')), labels=date_format("%Y"))
    ggsave(paste0(file_basename, "_SPI_", spi_period,
                  "_meantimeseries.png"),p1, width=4, height=2, dpi=PLOT_DPI)
    ggsave(paste0(file_basename, "_SPI_", spi_period,
                  "_meantimeseries.eps"), p1, width=4, height=2, dpi=PLOT_DPI)
}
