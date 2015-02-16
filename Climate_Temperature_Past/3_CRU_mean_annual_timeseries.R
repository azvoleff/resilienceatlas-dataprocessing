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
aoi_polygons <- aoi_polygons[aoi_polygons$Type == "Region", ]

temp_stats <- foreach (n=1:nrow(aoi_polygons), .inorder=FALSE,
                       .packages=c("rgdal", "lubridate", "dplyr",
                                   "raster", "foreach", "ggplot2",
                                   "scales", "rgeos", "teamlucc")) %do% {
    aoi <- aoi_polygons[n, ]
    name <- as.character(aoi$Name)
    name <- gsub(' ', '', name)
    aoi <- gConvexHull(aoi)
    aoi <- spTransform(aoi, CRS(utm_zone(aoi, proj4string=TRUE)))
    aoi <- gBuffer(aoi, width=100000)
    aoi <- spTransform(aoi, CRS('+init=epsg:4326'))

    annual_means <- foreach(dataset=datasets, .combine=rbind) %do% {
        filename_base <- paste0(name, '_', product, '_', dataset, '_')
        cru_data_file <- file.path(out_folder,
                              paste0(filename_base, datestring,  '.tif'))
        included_dates <- dates[(dates >= start_date) & (dates < end_date)]
        band_nums <- c(1:length(dates))[(dates >= start_date) & (dates < end_date)]
        cru_data <- stack(cru_data_file, bands=band_nums)

        aoi <- spTransform(aoi, CRS(proj4string(cru_data)))

        cru_data <- mask(cru_data, aoi)

        years <- year(included_dates)
        years_rep <- rep(years, each=nrow(cru_data)*ncol(cru_data))
        subyears <- rep(seq(1, num_periods),  length.out=nlayers(cru_data))
        subyears_rep <- rep(subyears, each=nrow(cru_data)*ncol(cru_data))
        pixels_rep <- rep(seq(1:(nrow(cru_data)*ncol(cru_data))), nlayers(cru_data))
        cru_data_df <- data.frame(year=years_rep,
                                  subyear=subyears_rep, 
                                  pixel=pixels_rep,
                                  cru_data=as.vector(cru_data))
        cru_data_df <- tbl_df(cru_data_df)

        annual_mean <- group_by(cru_data_df, year) %>%
            summarize(mean=mean(cru_data, na.rm=TRUE))
        annual_mean <- data.frame(cbind(dataset=dataset, annual_mean))
        return(annual_mean)
    }

    write.csv(annual_means,
              file=file.path(out_folder, paste0(name, "_", product, 
                                                '_meanannual_timeseries.csv')),
              row.names=FALSE)

    annual_means$dataset <- as.character(annual_means$dataset)
    annual_means$dataset[annual_means$dataset == "tmp"] <- "Mean"
    annual_means$dataset[annual_means$dataset == "tmn"] <- "Minimum"
    annual_means$dataset[annual_means$dataset == "tmx"] <- "Maximum"

    p1 <- ggplot(annual_means) +
        theme_bw() +
        geom_line(aes(year, mean, colour=dataset)) +
        xlab('Year') + ylab(expression('Temperature ('*degree*'C)')) +
        scale_x_continuous(breaks=c(1984, 2000, 2014)) +
        scale_colour_discrete("Temperature")
    ggsave(file.path(out_folder, paste0(name, "_", product, 
                                        '_meanannual_timeseries.png')), p1,
           width=4, height=2, dpi=PLOT_DPI)
    ggsave(file.path(out_folder, paste0(name, "_", product, 
                                        '_meanannual_timeseries.eps')), p1,
           width=4, height=2, dpi=PLOT_DPI)

    p2 <- ggplot(filter(annual_means, dataset == "Mean")) +
        theme_bw() +
        geom_line(aes(year, mean), colour="coral3") +
        xlab('Year') + ylab(expression('Temperature ('*degree*'C)')) +
        scale_x_continuous(breaks=c(1984, 2000, 2014))
    ggsave(file.path(out_folder, paste0(name, "_", product, 
                                        '_meanannual_timeseries_tmponly.png')), p2,
           width=4, height=2, dpi=PLOT_DPI)
    ggsave(file.path(out_folder, paste0(name, "_", product, 
                                        '_meanannual_timeseries_tmponly.eps')), p2,
           width=4, height=2, dpi=PLOT_DPI)
}

stopCluster(cl)
