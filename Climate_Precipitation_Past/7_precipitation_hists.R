library(raster)
library(maptools)
library(foreach)
library(doParallel)

cl  <- makeCluster(3)
registerDoParallel(cl)

source('../0_settings.R')

shp_folder <- file.path(prefix, "GRP", "Boundaries")
stopifnot(file_test('-d', shp_folder))
chirps_folder <- file.path(prefix, "GRP", "CHIRPS-2.0")
stopifnot(file_test('-d', chirps_folder))

countries <- read.csv(file.path(prefix, "GRP", "DataTables", "GRP_Countries.csv"))
regions <- read.csv(file.path(prefix, "GRP", "DataTables", "GRP_Regions.csv"))
countries <- merge(countries, regions)
countries$Region_Name <- gsub(' ', '', countries$Region_Name)

aoi_polygons <- readShapeSpatial(file.path(shp_folder, 'GRP_Countries.shp'))
monthly_precip <- foreach(region=unique(countries$Region_Name), 
                          .inorder=FALSE, .combine=rbind) %do% {
    timestamp()
    print(region)
    chirps_file <- file.path(chirps_folder, paste0(region, '_CHIRPS_monthly_198101-201412_mean_monthly.gri'))
    chirps <- brick(chirps_file)
    these_countries <- aoi_polygons[aoi_polygons$ISO3 %in% countries[countries$Region_Name == region, ]$ISO3, ]
    out <- foreach (n=1:nrow(these_countries), .inorder=FALSE,
             .packages=c('raster'), .combine=rbind) %dopar% {
        this_country <- these_countries[n, ]
        this_chirps <- crop(chirps, this_country)
        this_chirps <- mask(this_chirps, this_country)
        mean_monthly <- cellStats(this_chirps, 'mean', na.rm=TRUE)
        return(data.frame(ISO3=this_country$ISO3,
                          period='1981-2014',
                          month=month.abb,
                          precip=mean_monthly))
    }
    return(out)
}
timestamp()

library(ggplot2)
library(dplyr)
write.csv(monthly_precip,
          file=file.path(chirps_folder, 'monthly_mean_precip_1981-2014.csv'), 
          row.names=FALSE)

monthly_precip$month <- ordered(monthly_precip$month, levels=month.abb)
foreach(this_ISO3=unique(monthly_precip$ISO3)) %do% {
    ggplot(filter(monthly_precip, ISO3 == this_ISO3)) +
        geom_bar(aes(month, precip), stat='identity') +
        xlab('Month') + ylab('Precipitation (mm)')
    ggsave(file.path(chirps_folder,
                     paste0('monthly_mean_precip_1981-2014_', this_ISO3, '.png')),
           height=3, width=4, dpi=100)
}
