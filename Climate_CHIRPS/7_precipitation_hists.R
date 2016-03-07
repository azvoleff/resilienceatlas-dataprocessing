library(raster)
library(maptools)
library(foreach)
library(doParallel)

cl  <- makeCluster(3)
registerDoParallel(cl)

source('../0_settings.R')

# shp_folder <- file.path(prefix, "GRP", "Boundaries")
# out_folder <- file.path(prefix, "GRP", "CHIRPS-2.0")
# aoi_polygons <- readShapeSpatial(file.path(shp_folder, 'GRP_Countries.shp'))
out_folder <- file.path(prefix, "Vital_Signs", "CHIRPS-2.0")
shp_folder <- file.path(prefix, "Vital_Signs", "Admin_Boundaries")
aoi_polygons <- readShapeSpatial(file.path(shp_folder, 'VS_Countries.shp'))
names(aoi_polygons)[names(aoi_polygons) == "ISO"] <- "ISO3"
stopifnot(file_test('-d', shp_folder))
stopifnot(file_test('-d', out_folder))

monthly_precip <- foreach (n=1:nrow(aoi_polygons), .inorder=FALSE,
                           .packages=c('raster'), .combine=rbind) %dopar% {
    timestamp()
    this_country <- aoi_polygons[n, ]
    chirps_file <- file.path(out_folder, paste0(this_country$ISO3, '_CHIRPS_monthly_198501-201412_mean_monthly.gri'))
    chirps <- brick(chirps_file)
    this_chirps <- crop(chirps, this_country)
    this_chirps <- mask(this_chirps, this_country)
    mean_monthly <- cellStats(this_chirps, 'mean', na.rm=TRUE)

    return(data.frame(ISO3=this_country$ISO3,
                      period='1985-2014',
                      month=month.abb,
                      precip=mean_monthly))
}
timestamp()
write.csv(monthly_precip,
          file=file.path(out_folder, 'monthly_mean_precip_1985-2014.csv'), 
          row.names=FALSE)

library(ggplot2)
library(dplyr)

monthly_precip$month <- ordered(monthly_precip$month, levels=month.abb)
foreach(this_ISO3=unique(monthly_precip$ISO3)) %do% {
    ggplot(filter(monthly_precip, ISO3 == this_ISO3)) +
        geom_bar(aes(month, precip), stat='identity') +
        xlab('Month') + ylab('Precipitation (mm)')
    ggsave(file.path(out_folder,
                     paste0('monthly_mean_precip_1981-2014_', this_ISO3, '.png')),
           height=3, width=4, dpi=100)
}
