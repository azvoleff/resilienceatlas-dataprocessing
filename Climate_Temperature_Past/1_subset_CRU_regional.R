###############################################################################
# Crops CRU pentad or monthly precipitation data to cover the spatial extent 
# of the shp/CSA/PA boundary of each team site.
###############################################################################

library(rgdal)
library(raster)
library(stringr)
library(gdalUtils)
library(rgeos)
library(teamlucc)
library(foreach)
library(doParallel)

cl <- makeCluster(3)
registerDoParallel(cl)

source('../0_settings.R')

n_cpus <- 4
overwrite <- TRUE

product <- 'cru_ts3.22'
datestring <- '1901.2013'

in_folder <- file.path(prefix, "CRU", "cru_ts_3.22")
out_folder <- file.path(prefix, "GRP", "CRU")
shp_folder <- file.path(prefix, "GRP", "Boundaries", "Regional")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))
stopifnot(file_test("-d", shp_folder))

datasets <- c('tmn', 'tmx', 'tmp', 'pet')

# This is the projection of the CRU files
s_srs <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'

region_polygons <- readOGR(shp_folder, 'GRP_regions')

foreach (dataset=datasets, .inorder=FALSE,
         .packages=c("teamlucc", "rgeos", "raster", "rgdal")) %dopar% {
    timestamp()
    message('Processing ', dataset, '...')

    ncdf <- file.path(in_folder, dataset,
                      pattern=paste(product, datestring, dataset, 'dat.nc', 
                                    sep='.'))
    this_dataset <- stack(ncdf)
    proj4string(this_dataset) <- s_srs

    stopifnot(file_test('-f', ncdf))

    for (n in 1:nrow(region_polygons)) {
        aoi <- region_polygons[n, ]
        region <- as.character(aoi$Region)
        region <- gsub(' ', '', region)
        aoi <- gConvexHull(aoi)
        aoi <- spTransform(aoi, CRS(utm_zone(aoi, proj4string=TRUE)))
        aoi <- gBuffer(aoi, width=100000)
        aoi <- spTransform(aoi, CRS(s_srs))

        dstfile <- file.path(out_folder,
                              paste0(region, "_", product, '_', dataset, '_', 
                                     datestring,  '.tif'))
        cropped_data <- crop(this_dataset, aoi, overwrite=TRUE, filename=dstfile)
    }
}

stopCluster(cl)
