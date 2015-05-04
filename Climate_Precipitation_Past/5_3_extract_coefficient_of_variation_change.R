source('../0_settings.R')

library(rgdal)
library(raster)
library(lubridate)
library(dplyr)
library(foreach)
library(doParallel)
library(RColorBrewer)
library(ggplot2)
library(gridExtra)
library(broom)

cl  <- makeCluster(3)
registerDoParallel(cl)

# For monthly data:
dataset <- 'monthly' # For SPI, use monthly

in_folder <- file.path(prefix, "GRP", "CHIRPS-2.0")
out_folder <- file.path(prefix, "GRP", "CHIRPS-2.0")
shp_folder <- file.path(prefix, "GRP", "Boundaries")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))
stopifnot(file_test('-d', shp_folder))

# Note the below code is INCLUSIVE of the start date
chirps_start_date <- as.Date('1981/1/1')
# Note the below code is INCLUSIVE of the end date
chirps_end_date <- as.Date('2014/12/1')
yrs <- seq(year(chirps_start_date), year(chirps_end_date))
dates <- seq(chirps_start_date, chirps_end_date, by='months')
periods_per_year <- 12

# Select the start and end dates for the data to include in this analysis
start_date <- as.Date('1981/1/1') # Inclusive
end_date <- as.Date('2014/12/1') # Exclusive

aoi_polygons <- readOGR(shp_folder, 'Analysis_Areas')
aoi_polygons <- aoi_polygons[aoi_polygons$Name == "Horn of Africa", ]

seasons <- c('Dry', 'Short rains', 'Long rains')

foreach (n=1:nrow(aoi_polygons), .inorder=FALSE,
         .packages=c("rgdal", "lubridate", "dplyr", "raster",
                     "rgeos")) %dopar% {
    timestamp()
    aoi <- aoi_polygons[n, ]
    name <- as.character(aoi$Name)
    name <- gsub(' ', '', name)

    print(paste0("Processing ", name, "..."))

    in_basename <- file.path(in_folder, paste0(name, '_CHIRPS'))

    out_basename <- file.path(out_folder, paste0(name, '_CHIRPS_',
                                format(start_date, "%Y%m"), '-', 
                                format(end_date, "%Y%m")))

    foreach (season=seasons,
             .packages=c('broom', 'dplyr', 'raster', 'rgdal',
                         'ggplot2')) %dopar% {
        this_basename <- paste0(out_basename, '_', paste(this_season, collapse=''))
        seasonal_ppt <- brick(paste0(this_basename, '_total.tif'))

        # Calculate coefficient of variation for the first half and the end of 
        # the period
        first_half <- stack(seasonal_ppt, layers=as.integer(c(1:floor(nlayers(seasonal_ppt) / 2))))
        second_half <- stack(seasonal_ppt, layers=as.integer(c(floor(nlayers(seasonal_ppt) / 2):nlayers(seasonal_ppt))))
        cv_1 <- cv(first_half)
        cv_2 <- cv(second_half)

        slope_rast <- writeRaster(cv_2 - cv_1, filename=paste0(this_basename, 
                                                               '_coefvariation_change.tif'), 
                                  overwrite=TRUE)
    }
    
}

stopCluster(cl)
