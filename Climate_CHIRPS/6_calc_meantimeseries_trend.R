source('../0_settings.R')

library(rgdal)
library(raster)
library(lubridate)
library(dplyr)
library(rgeos)
library(foreach)
library(pracma)

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
aoi_polygons <- aoi_polygons[aoi_polygons$Type != "Region", ]

ppt_trends <- foreach (n=1:nrow(aoi_polygons), .combine=rbind) %do% {
    aoi <- aoi_polygons[n, ]
    name <- as.character(aoi$Name)
    name <- gsub(' ', '', name)

    in_basename <- file.path(out_folder, paste0(name, '_CHIRPS_',
                             format(start_date, "%Y%m"), '-', 
                             format(end_date, "%Y%m")))
    annual_ppt <- read.csv(paste0(in_basename, '_meanannualppt.csv'))

    model <- lm(mean_annual ~ year, data=annual_ppt)
    d <- data.frame(summary(model)$coefficients[, c(1, 4)])
    d <- cbind(row.names(d), d)
    names(d) <- c('coef', 'estimate', 'p_val')
    row.names(d) <- NULL

    # Convert trend into fraction of mean
    d$estimate[d$coef == "year"] <- d$estimate[d$coef == "year"] / mean(annual_ppt$mean_annual)
    # Convert trend into fraction / decade
    d$estimate[d$coef == "year"] <- d$estimate[d$coef == "year"]  * 10
    # Convert trend into percent
    d$estimate[d$coef == "year"] <- d$estimate[d$coef == "year"]  * 100
    d <- d[d$coef == "year", ]
    d <- d[!names(d) == "coef"]
    d <- cbind(name=name, d)

    return(d)
}

ppt_trends$name <- as.character(ppt_trends$name)
ppt_trends <- ppt_trends[order(ppt_trends$name), ]

write.csv(ppt_trends, file="precipitation_trends.csv", row.names=FALSE)

ppt_var <- foreach (n=1:nrow(aoi_polygons), .combine=rbind) %do% {
    aoi <- aoi_polygons[n, ]
    name <- as.character(aoi$Name)
    name <- gsub(' ', '', name)

    in_basename <- file.path(out_folder, paste0(name, '_CHIRPS_',
                             format(start_date, "%Y%m"), '-', 
                             format(end_date, "%Y%m")))
    annual_ppt <- read.csv(paste0(in_basename, '_meanannualppt.csv'))

    d <- data.frame(name=name,
                    cv=cv(annual_ppt$mean_annual),
                    mean=mean(annual_ppt$mean_annual),
                    sd=sd(annual_ppt$mean_annual))
    return(d)
}

ppt_var$name <- as.character(ppt_var$name)
ppt_var <- ppt_var[order(ppt_var$name), ]

write.csv(ppt_var, file="precipitation_variability.csv", row.names=FALSE)

ppt_var_trend <- foreach (n=1:nrow(aoi_polygons), .combine=rbind) %do% {
    aoi <- aoi_polygons[n, ]
    name <- as.character(aoi$Name)
    name <- gsub(' ', '', name)

    in_basename <- file.path(out_folder, paste0(name, '_CHIRPS_',
                             format(start_date, "%Y%m"), '-', 
                             format(end_date, "%Y%m")))
    annual_ppt <- read.csv(paste0(in_basename, '_meanannualppt.csv'))

    this_ppt <- detrend(annual_ppt$mean_annual) + mean(annual_ppt$mean_annual)

    middle <- round(length(this_ppt)/2)

    cv_diffs <- foreach (i=1:1000, .combine=c) %do% {
        cv(sample(this_ppt[1:middle], replace=TRUE)) - 
        cv(sample(this_ppt[(middle + 1):length(this_ppt)], replace=TRUE))
    }

    d <- data.frame(name=name,
                    cv_diff=mean(cv_diffs),
                    lower=quantile(cv_diffs, c(.05)),
                    upper=quantile(cv_diffs, c(.95)))
    return(d)
}

ppt_var_trend$name <- as.character(ppt_var_trend$name)
ppt_var_trend <- ppt_var_trend[order(ppt_var_trend$name), ]

write.csv(ppt_var_trend, file="precipitation_variability_trend.csv", row.names=FALSE)
