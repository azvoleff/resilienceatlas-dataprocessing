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

n_cpus <- 3

cl  <- makeCluster(n_cpus)
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
chirps_end_date <- as.Date('2014/4/1')
yrs <- seq(year(chirps_start_date), year(chirps_end_date))
dates <- seq(chirps_start_date, chirps_end_date, by='months')
periods_per_year <- 12

# Select the start and end dates for the data to include in this analysis
start_date <- as.Date('1984/1/1') # Inclusive
end_date <- as.Date('2013/12/1') # Exclusive

aoi_polygons <- readOGR(shp_folder, 'Analysis_Areas')
aoi_polygons <- aoi_polygons[aoi_polygons$Type == "Landscape", ]

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

    # Map areas that are getting signif. wetter or drier, coded by mm per year
    extract_coefs <- function(model) {
        d <- data.frame(summary(model)$coefficients[, c(1, 4)])
        d <- cbind(row.names(d), d)
        names(d) <- c('coef', 'estimate', 'p_val')
        row.names(d) <- NULL
        return(d)
    }
    annual_ppt_lm_coefs <- group_by(chirps_df, year, pixel) %>%
        summarize(ppt_annual=sum(ppt, na.rm=TRUE)) %>%
        group_by(pixel) %>%
        do(extract_coefs(lm(ppt_annual ~ year, data=.)))

    # Use chirps raster as a template
    annual_ppt_slope_rast <- brick(chirps, values=FALSE, nl=1)
    annual_ppt_slope_rast <- setValues(annual_ppt_slope_rast,
                               matrix(filter(annual_ppt_lm_coefs, coef == "year")$estimate, 
                                      nrow=nrow(chirps)*ncol(chirps), 
                                      ncol=1, byrow=TRUE))
    annual_ppt_slope_rast <- writeRaster(annual_ppt_slope_rast,
                filename=paste0(out_basename, '_annual_ppt_slope.tif'), 
                overwrite=TRUE)

    annual_ppt_p_val_rast <- brick(chirps, values=FALSE, nl=1)
    annual_ppt_p_val_rast <- setValues(annual_ppt_p_val_rast,
                               matrix(filter(annual_ppt_lm_coefs, coef == "year")$p_val, 
                                      nrow=nrow(chirps)*ncol(chirps), 
                                      ncol=1, byrow=TRUE))
    annual_ppt_p_val_rast <- writeRaster(annual_ppt_p_val_rast,
                filename=paste0(out_basename, '_annual_ppt_pval.tif'), 
                overwrite=TRUE)

    annual_ppt_slope_rast <- overlay(annual_ppt_slope_rast, annual_ppt_p_val_rast,
        fun=function(slp, p) {
            return(slp * (p < .05))
        },
        filename=paste0(out_basename, '_annual_ppt_slope_masked.tif'),
        overwrite=TRUE)

}

stopCluster(cl)
