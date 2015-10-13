library(raster)
library(rgdal)
library(foreach)
library(doParallel)
library(rgeos)

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

aoi_polygons <- readOGR(shp_folder, 'GRP_Countries')
monthly_precip <- foreach(region=unique(countries$Region_Name_Short), 
                          .inorder=FALSE, .combine=rbind) %do% {
    timestamp()
    print(region)
    chirps_file <- file.path(chirps_folder, paste0(region, '_CHIRPS_monthly_198101-201412_mean_monthly.grd'))
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
                          month=month.abb))
    }
    return(out)
}
timestamp()

write.csv(monthly_precip,
          filename=file.path(chirps_folder, 'monthly_mean_precip_1981-2014.csv'), 
          row.names=FALSE)

