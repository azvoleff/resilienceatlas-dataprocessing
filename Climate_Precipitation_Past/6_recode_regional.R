###############################################################################
# Calculates mean precipitation for GRP countries.
###############################################################################

source('../0_settings.R')

library(raster)
library(spatial.tools)
library(lubridate)

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
aoi_polygons <- aoi_polygons[aoi_polygons$Type == "Landscape", ]

foreach (n=1:nrow(aoi_polygons), .inorder=FALSE,
         .packages=c("rgdal", "lubridate", "dplyr", "raster",
                     "rgeos", "teamlucc")) %do% {
    timestamp()

    aoi <- aoi_polygons[n, ]
    name <- as.character(aoi$Name)
    name <- gsub(' ', '', name)

    message('Processing ', name, '...')

    out_basename <- file.path(out_folder, paste0(name, '_CHIRPS_',
                                format(start_date, "%Y%m"), '-', 
                                format(end_date, "%Y%m")))

    ##########################################################################
    # Recode annual mean precipitation

    # Below breaks are for IDN only
    # annual_mean_ppt_rclmat <- matrix(c(-Inf, 0, -1,
    #                                    0, 500, 0,
    #                                    500, 1000, 1,
    #                                    1000, 1500, 2,
    #                                    1500, 2000, 3,
    #                                    2000, 2500, 4,
    #                                    2500, 3000, 5,
    #                                    3000, 3500, 6,
    #                                    3500, 4000, 7,
    #                                    4000, 4500, 8,
    #                                    4500, 5000, 9,
    #                                    5000, 5500, 10,
    #                                    5500, Inf, 11), ncol=3, byrow=TRUE)
    annual_mean_ppt_rclmat <- matrix(c(-Inf, 0, -1,
                                       0, 250, 0,
                                       250, 500, 1,
                                       500, 750, 2,
                                       750, 1000, 3,
                                       1000, 1250, 4,
                                       1250, 1500, 5,
                                       1500, 2000, 6,
                                       2000, 2500, 7,
                                       2000, 3000, 8,
                                       3000, Inf, 9), ncol=3, byrow=TRUE)
    annual_mean_ppt_file <- paste0(out_basename, '_ppt_mean_12mth.tif')
    annual_mean_ppt <- raster(annual_mean_ppt_file)
    annual_mean_ppt_reclass <- reclassify(annual_mean_ppt, annual_mean_ppt_rclmat)

    annual_mean_ppt_rcldf <- data.frame(annual_mean_ppt_rclmat)
    annual_mean_ppt_rcldf [1, 1:2] <- NA
    names(annual_mean_ppt_rcldf) <- c("lowvalue", "highvalue", "code")

    annual_mean_ppt_classifed_file <- paste0(out_basename, '_ppt_mean_12mth_classified.tif')
    annual_mean_ppt_classes_file <- paste0(out_basename, '_ppt_mean_12mth_classified_classkey.csv')
    write.csv(annual_mean_ppt_rcldf, file=annual_mean_ppt_classes_file, 
              row.names=FALSE)
    writeRaster(annual_mean_ppt_reclass, 
                filename=annual_mean_ppt_classifed_file, overwrite=TRUE)

    ##########################################################################
    # Recode annual mean precipitation
    annual_ppt_slope <- raster(paste0(out_basename, '_annual_ppt_slope_masked.tif'))
    annual_ppt_p_val <- raster(paste0(out_basename, '_annual_ppt_pval.tif'))

    # Convert to mm per decade
    annual_ppt_slope <- annual_ppt_slope * 10

    # Convert to percentage of current mean rainfall (while ignoring mean 
    # rainfall less than one, which is missing data)
    annual_ppt_slope <- annual_ppt_slope / (annual_mean_ppt * (annual_mean_ppt > 0))

    # Drop insignificant trends
    annual_ppt_slope <- annual_ppt_slope * (annual_ppt_p_val <.05)

    annual_ppt_slope_rclmat <- matrix(c(-Inf, -.1, 0,
                                        -.1, -.05, 1,
                                        -.05, 0, 2,
                                        0, .05, 3,
                                        .05, .1, 4,
                                        .1, Inf, 5), ncol=3, byrow=TRUE)
    annual_ppt_slope_reclass <- reclassify(annual_ppt_slope, annual_ppt_slope_rclmat)

    # Code NA values
    annual_ppt_slope_reclass[is.na(annual_ppt_slope_reclass )] <- -1

    annual_ppt_slope_rcldf <- data.frame(annual_ppt_slope_rclmat)
    names(annual_ppt_slope_rcldf) <- c("lowvalue", "highvalue", "code")

    annual_ppt_slope_classifed_file <- paste0(out_basename, '_ppt_10yrslope_classified.tif')
    annual_ppt_slope_classes_file <- paste0(out_basename, '_ppt_10yrslope_classified_classkey.csv')
    write.csv(annual_ppt_slope_rcldf, file=annual_ppt_slope_classes_file, 
              row.names=FALSE)
    writeRaster(annual_ppt_slope_reclass, filename=annual_ppt_slope_classifed_file,
                overwrite=TRUE)

    ##########################################################################

    return(TRUE)
}
