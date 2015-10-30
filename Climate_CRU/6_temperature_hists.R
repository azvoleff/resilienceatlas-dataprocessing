###############################################################################
# Calculates mean precipitation for GRP countries.
###############################################################################

source('../0_settings.R')

library(rgdal)
library(raster)
library(lubridate)
library(dplyr)
library(foreach)
library(doParallel)
library(maptools)
library(spatial.tools)

n_cpus <- 30

cl  <- makeCluster(n_cpus)
registerDoParallel(cl)

product <- 'cru_ts3.23'
datestring <- '19810101-20141231'

# Note the below code is INCLUSIVE of the start date
cru_start_date <- as.Date('1981/1/1')
# Note the below code is EXCLUSIVE of the end date
cru_end_date <- as.Date('2015/1/1')

yrs <- seq(year(cru_start_date), year(cru_end_date))
dates <- seq(cru_start_date, cru_end_date, by='months')
dates <- dates[dates < cru_end_date]
num_periods <- 12

# Choose a start and end year for the data to include in this analysis
start_date <- as.Date('1985/1/1') # Inclusive
end_date <- as.Date('2015/1/1') # Exclusive

datasets <- c('tmp', 'tmn', 'tmx', 'pre')

comparison_period <- '1985-2015'

in_folder <- file.path(prefix, "GRP", "CRU")
out_folder <- file.path(prefix, "GRP", "CRU")
shp_folder <- file.path(prefix, "GRP", "Boundaries")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))
stopifnot(file_test('-d', shp_folder))

country_polygons <- readShapeSpatial(file.path(shp_folder, 'GRP_Countries.shp'))

foreach (dataset=datasets, .inorder=FALSE,
         .packages=c("rgdal", "lubridate", "dplyr", "raster")) %do% {
    timestamp()
    print(paste('Processing', dataset, '...'))
    filename_base <- paste0('Global', '_', product, '_', dataset, '_')
    cru_data_file <- file.path(out_folder,
                          paste0(filename_base, datestring,  '.tif'))

    # Calculate the band numbers that are needed
    included_dates <- dates[(dates >= start_date) & (dates < end_date)]
    band_nums <- c(1:length(dates))[(dates >= start_date) & (dates < end_date)]

    cru_data <- stack(cru_data_file, bands=band_nums)

    hists <- foreach (n=1:nrow(country_polygons), .inorder=FALSE,
                      .packages=c('raster', 'foreach'), .combine=rbind) %dopar% {
        this_country <- country_polygons[n, ]
        this_cru <- crop(cru_data, this_country)
        this_cru <- mask(this_cru, this_country)
        mthly_mean <- foreach(n=1:12, .combine=c) %do% {
            # Pull out all arrays for this month and calculate mean
            cellStats(raster::mean(this_cru[[seq(from=n, by=12, length.out=round(dim(this_cru)[3]/12))]]), 'mean')
        }
        return(data.frame(ISO3=this_country$ISO3,
                          variable=dataset,
                          period=comparison_period,
                          month=month.abb,
                          value=mthly_mean))
    }

    write.csv(hists,
              file=file.path(in_folder, paste('cru_ts3.23', dataset, comparison_period, 
                         'monthly.csv', sep='_')),
              row.names=FALSE)


    return(out)
}
timestamp()

# library(ggplot2)
# library(dplyr)
# write.csv(monthly_precip,
#           file=file.path(chirps_folder, 'monthly_mean_precip_1981-2014.csv'), 
#           row.names=FALSE)

# monthly_precip$month <- ordered(monthly_precip$month, levels=month.abb)
# foreach(this_ISO3=unique(monthly_precip$ISO3)) %do% {
#     ggplot(filter(monthly_precip, ISO3 == this_ISO3)) +
#         geom_bar(aes(month, precip), stat='identity') +
#         xlab('Month') + ylab('Precipitation (mm)')
#     ggsave(file.path(chirps_folder,
#                      paste0('monthly_mean_precip_1981-2014_', this_ISO3, '.png')),
#            height=3, width=4, dpi=100)
# }
