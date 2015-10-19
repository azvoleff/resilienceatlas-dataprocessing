source('../0_settings.R')

library(rgdal)
library(raster)
library(lubridate)
library(dplyr)
library(foreach)
library(doParallel)
library(spatial.tools)
library(ggplot2)
library(scales)

n_cpus <- 3

cl  <- makeCluster(n_cpus)
registerDoParallel(cl)

product <- 'cru_ts3.22'
datestring <- '1901.2013'

# Note the below code is INCLUSIVE of the start date
cru_start_date <- as.Date('1901/1/1')
# Note the below code is EXCLUSIVE of the end date
cru_end_date <- as.Date('2014/1/1')

yrs <- seq(year(cru_start_date), year(cru_end_date))
dates <- seq(cru_start_date, cru_end_date, by='months')
dates <- dates[dates < cru_end_date]
num_periods <- 12

# Choose a start and end year for the data to include in this analysis
start_date <- as.Date('1984/1/1') # Inclusive
end_date <- as.Date('2014/1/1') # Exclusive

datasets <- c('tmp', 'tmn', 'tmx')

in_folder <- file.path(prefix, "GRP", "CRU")
out_folder <- file.path(prefix, "GRP", "CRU")
shp_folder <- file.path(prefix, "GRP", "Boundaries")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))
stopifnot(file_test('-d', shp_folder))

aoi_polygons <- readOGR(shp_folder, 'Analysis_Areas')

temp_trends <- temp_stats <- foreach (n=1:nrow(aoi_polygons), .inorder=FALSE,
                       .packages=c("rgdal", "lubridate", "dplyr",
                                   "raster", "foreach", "ggplot2",
                                   "scales", "rgeos", "teamlucc"),
                       .combine=rbind) %do% {
    aoi <- aoi_polygons[n, ]
    name <- as.character(aoi$Name)
    name <- gsub(' ', '', name)

    annual_means <- read.csv(file.path(out_folder, paste0(name, "_", product, '_meanannual_timeseries.csv')))

    annual_means$dataset <- as.character(annual_means$dataset)
    annual_means$dataset[annual_means$dataset == "tmp"] <- "Mean"
    annual_means$dataset[annual_means$dataset == "tmn"] <- "Minimum"
    annual_means$dataset[annual_means$dataset == "tmx"] <- "Maximum"

    model <- lm(mean ~ year, data=filter(annual_means, dataset == "Mean"))
    d <- data.frame(summary(model)$coefficients[, c(1, 4)])
    d <- cbind(row.names(d), d)
    names(d) <- c('coef', 'estimate', 'p_val')
    row.names(d) <- NULL
    # Convert trend into degrees / decade
    d <- d[d$coef == "year", ]
    d$estimate[d$coef == "year"] <- d$estimate[d$coef == "year"] * 10
    d <- d[!names(d) == "coef"]
    d <- cbind(name=name, d)
    return(d)
}

temp_trends$name <- as.character(temp_trends$name)
temp_trends <- temp_trends[order(temp_trends$name), ]

write.csv(temp_trends, file="temperature_trends.csv", row.names=FALSE)

stopCluster(cl)
