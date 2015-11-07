###############################################################################
# Calculates mean precipitation for GRP countries.
###############################################################################

source('../0_settings.R')

library(tools)
library(stringr)
library(raster)
library(lubridate)
library(dplyr)
library(rgeos)
library(foreach)
library(doParallel)
library(spatial.tools)
library(maptools)

# Select the start and end dates for the data to include in this analysis
start_date <- as.Date('1985/1/1') # Inclusive
end_date <- as.Date('2014/12/1') # Exclusive

cl  <- makeCluster(3)
registerDoParallel(cl)

in_folder <- file.path(prefix, "GRP", "CHIRPS-2.0")
shp_folder <- file.path(prefix, "GRP", "Boundaries")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', shp_folder))

countries <- read.csv(file.path(prefix, "GRP", "DataTables", "GRP_Countries.csv"))
regions <- read.csv(file.path(prefix, "GRP", "DataTables", "GRP_Regions.csv"))
countries <- merge(countries, regions)
countries$Region_Name <- gsub(' ', '', countries$Region_Name)

# Read in seasonal assignments
season_key <- read.csv(file.path(prefix, "GRP", "DataTables", "Rainy_Seasons.csv"), stringsAsFactors=FALSE)

# Function to calculate trend
calc_decadal_trend <- function(p, season_IDs, ...) {
    p[p == -9999] <- NA
    # Setup period identifiers so the data can be used in a dataframe
    season_IDs_rep <- rep(season_IDs, each=dim(p)[1]*dim(p)[2])
    pixels_rep <- rep(seq(1:(dim(p)[1]*dim(p)[2])), dim(p)[3])
    p_df <- data.frame(season_ID=season_IDs_rep,
                       pixel=pixels_rep,
                       ppt=as.vector(p))
    # Map areas that are getting signif. wetter or drier, coded by mm per year
    extract_coefs <- function(indata) {
        if (sum(!is.na(indata$ppt_annual_pctmean)) < 3) {
            d <- data.frame(coef=c('(Intercept)', 'season_ID'), c(NA, NA), c(NA, NA))
        } else {
            model <- lm(ppt_annual_pctmean ~ season_ID, data=indata)
            d <- data.frame(summary(model)$coefficients[, c(1, 4)])
            d <- cbind(row.names(d), d)
        }
        names(d) <- c('coef', 'estimate', 'p_val')
        row.names(d) <- NULL
        return(d)
    }
    lm_coefs <- group_by(p_df, season_ID, pixel) %>%
        summarize(ppt_annual=sum(ppt, na.rm=TRUE)) %>%
        group_by(pixel) %>%
        mutate(ppt_annual_pctmean=(ppt_annual/mean(ppt_annual))*100) %>%
        do(extract_coefs(.))
    # The *10 below is to convert to decadal change
    out <- array(c(filter(lm_coefs, coef == "season_ID")$estimate * 10,
                   filter(lm_coefs, coef == "season_ID")$p_val),
                 dim=c(dim(p)[1], dim(p)[2], 2))
    # Mask out nodata areas
    out[ , , 1][is.na(p[ , , 1])] <- NA
    out[ , , 2][is.na(p[ , , 1])] <- NA
    out
}

# Function that will create sequences of month numbers even when they wrap 
# around the end of a year (for example, 11-4 where that is November-April).
seq_month_wrap <- function(t0, tf) {
    if (t0 == tf) {
        return(t0)
    } else if (t0 < tf) {
        return(c(t0:tf))
    } else {
        return(c(t0:12, 1:tf))
    }
}

# Function to give position of last match in a series
last_match <- function(x, y) {
    return(length(y) - match(x, rev(y)) + 1)
}

aoi_polygons <- readShapeSpatial(file.path(shp_folder, 'GRP_Countries.shp'))

# Note that seasonal total rainfall is returned for the purposes of sorting the 
# layers for later display
seasonal_totals <- foreach(region=unique(countries$Region_Name), 
                           .inorder=FALSE, .combine=rbind) %do% {
    chirps_file <- file.path(in_folder,
                             paste0(region, '_CHIRPS_monthly_198101-201412.tif'))
    timestamp()
    print(region)
    these_countries <- aoi_polygons[aoi_polygons$ISO3 %in% countries[countries$Region_Name == region, ]$ISO3, ]
    seasonal_totals <- foreach (n=1:nrow(these_countries), .inorder=FALSE,
             .packages=c('raster', 'stringr', 'dplyr', 'spatial.tools', 
                         'rgdal', 'lubridate', 'tools'),
             .combine=rbind) %dopar% {
        this_country <- these_countries[n, ]

        name <- str_extract(basename(chirps_file), '^[a-zA-Z]*')

        # Read season 1
        inc_subyrs_str_1 <- season_key[season_key$ISO3 == this_country$ISO3, ]$RS_1_Months
        t0 <- as.numeric(str_extract(inc_subyrs_str_1, '^[0-9]*'))
        tf <- as.numeric(str_extract(inc_subyrs_str_1, '[0-9]*$'))
        inc_subyrs_1 <- seq_month_wrap(t0, tf)
        stopifnot((min(inc_subyrs_1) >= 1) & max(inc_subyrs_1) <=12)
        seasons <- list(inc_subyrs_1)

        # Read season 2. But not all countries have a season 2.
        inc_subyrs_str_2 <- season_key[season_key$ISO3 == this_country$ISO3, ]$RS_2_Months
        if (inc_subyrs_str_2 != '') {
            t0 <- as.numeric(str_extract(inc_subyrs_str_2, '^[0-9]*'))
            tf <- as.numeric(str_extract(inc_subyrs_str_2, '[0-9]*$'))
            inc_subyrs_2 <- seq_month_wrap(t0, tf)
            stopifnot((min(inc_subyrs_2) >= 1) & max(inc_subyrs_2) <=12)
            seasons <- list(inc_subyrs_1, inc_subyrs_2)
        }

        seasonal_totals <- foreach (season=seasons, .combine=rbind) %do% {
            # Calculate the band numbers that are needed
            dates <- seq(as.Date('1981/1/1'), as.Date('2014/12/1'), by='months')
            band_nums <- c(1:length(dates))[(dates >= start_date) & (dates <= end_date)]
            dates <- dates[band_nums]
            # Extract only band numbers for the included months, so we are not 
            # read/writing extra data
            which_dates <- which(month(dates) %in% season)
            # Ensure partial seasons aren't included at the ends of the 
            # timeseries
            which_dates <- which_dates[which_dates >= match(season[1], month(dates))]
            which_dates <- which_dates[which_dates <= last_match(season[length(season)], month(dates))]
            band_nums <- band_nums[which_dates]
            dates <- dates[which_dates]

            # Check there are the same number of occurrences for each month
            stopifnot(length(unique(table(month(dates)))) == 1)
            n_years <- unique(table(month(dates)))

            # Setup a sequence of season identifiers so bands can be tied to a 
            # particular season
            season_IDs <- rep(1:n_years, each=length(season))

            season_string <- paste0('_', paste(season, collapse='-'))

            start_date_text <- format(start_date, '%Y%m%d')
            end_date_text <- format(end_date, '%Y%m%d')
            out_basename <- paste0(gsub('[0-9-]{6}-[0-9-]{6}', '', 
                                        file_path_sans_ext(chirps_file)),
                start_date_text, '-', end_date_text, '_', this_country$ISO3)

            chirps <- stack(chirps_file, bands=band_nums)
            this_chirps <- crop(chirps, this_country)
            this_chirps <- mask(this_chirps, this_country)

            decadal_trend <- rasterEngine(p=this_chirps,
                args=list(season_IDs=season_IDs),
                fun=calc_decadal_trend, datatype='FLT4S', outbands=2, outfiles=1, 
                processing_unit="chunk",
                filename=paste0(out_basename, '_trend_decadal', season_string),
                .packages=c('dplyr', 'lubridate'))
            writeRaster(decadal_trend,
                        filename=paste0(out_basename, '_trend_decadal', 
                                        season_string, '_geotiff.tif'),
                        overwrite=TRUE)

            # Calculate total precip within this season
            seasonal_totals <- foreach(season_ID=unique(season_IDs), 
                                       .combine=c, .final=raster::stack) %do% {
                sum(stack(this_chirps, layers=which(season_IDs == season_ID)))
            }
            seasonal_total <- mean(seasonal_totals)
            writeRaster(seasonal_total,
                        filename=paste0(out_basename, '_meantotalppt_', 
                                        season_string, '_geotiff.tif'),
                        overwrite=TRUE)
            seasonal_total_mean <- cellStats(seasonal_total, 'mean')
            return(data.frame(ISO3=this_country$ISO3,
                              season=paste(season, collapse=' '), 
                              ppt=seasonal_total_mean))

        }
    }
}

write.csv(seasonal_totals,
          file=file.path(in_folder, 'seasonal_precipitation_totals.csv'), 
          row.names=FALSE)

stopCluster(cl)
