###############################################################################
# Calculates mean precipitation for GRP countries.
###############################################################################

source('../0_settings.R')

library(rgdal)
library(raster)
library(lubridate)
library(dplyr)
library(rgeos)
library(teamlucc)
library(foreach)
library(doParallel)
library(spatial.tools)

cl  <- makeCluster(10)
registerDoParallel(cl)

# For monthly data:
dataset <- 'monthly' # For SPI, use monthly

in_folder <- file.path(prefix, "GRP", "CHIRPS")
out_folder <- file.path(prefix, "GRP", "CHIRPS")
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
start_date <- as.Date('1985/1/1') # Inclusive
end_date <- as.Date('2014/12/1') # Exclusive

aoi_polygons <- readOGR(shp_folder, 'Analysis_Areas')

foreach (n=1:nrow(aoi_polygons), .inorder=FALSE,
         .packages=c("rgdal", "lubridate", "dplyr", "raster",
                     "rgeos", "teamlucc")) %dopar% {
    timestamp()
    aoi <- aoi_polygons[n, ]
    name <- as.character(aoi$Name)
    name <- gsub(' ', '', name)

    print(paste0("Processing ", name, "..."))

    in_basename <- file.path(in_folder, paste0(name, '_CHIRPS'))

    out_basename <- file.path(out_folder, paste0(name, '_CHIRPS_',
                                format(start_date, "%Y%m"), '-', 
                                format(end_date, "%Y%m")))
                                  
    chirps_tif <- paste0(in_basename, "_", dataset, '_', 
                         format(chirps_start_date, "%Y%m"), '-', 
                         format(chirps_end_date, "%Y%m"), '.tif')

    # Calculate the band numbers that are needed
    included_dates <- dates[(dates >= start_date) & (dates <= end_date)]
    band_nums <- c(1:length(dates))[(dates >= start_date) & (dates <= end_date)]

    chirps <- stack(chirps_tif, bands=band_nums)
    chirps <- mask(chirps, aoi)

    # Setup a dataframe with the precipitation data so anomalies, etc. can be 
    # calculated
    years <- year(included_dates)
    years_rep <- rep(years, each=nrow(chirps)*ncol(chirps))
    subyears <- rep(seq(1, periods_per_year),  length.out=nlayers(chirps))
    subyears_rep <- rep(subyears, each=nrow(chirps)*ncol(chirps))
    pixels_rep <- rep(seq(1:(nrow(chirps)*ncol(chirps))), nlayers(chirps))
    chirps_df <- data.frame(year=years_rep,
                            subyear=subyears_rep, 
                            pixel=pixels_rep,
                            ppt=as.vector(chirps))
    chirps_df <- tbl_df(chirps_df)
    chirps_df$ppt[chirps_df$ppt < 0] <- NA

    annual_ppt <- group_by(chirps_df, year, pixel) %>%
        summarise(annual=sum(ppt)) %>%
        group_by(year) %>%
        summarise(mean_annual=sum(annual, na.rm=TRUE)/sum(!is.na(annual)))

    write.csv(annual_ppt, file=paste0(out_basename, 
                                      '_meanannualppt.csv'), row.names=FALSE)

}

stopCluster(cl)
