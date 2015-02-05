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

overwrite <- TRUE

iso_key <- read.csv(file.path("..", "ISO_Codes.csv"))

product <- 'cru_ts3.22'
datestring <- '1901.2013'

in_folder <- file.path(prefix, "CRU", "cru_ts_3.22")
out_folder <- file.path(prefix, "GRP", "CRU")
shp_folder <- file.path(prefix, "GRP", "Boundaries", "National")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))
stopifnot(file_test("-d", shp_folder))

#datasets <- c('tmn', 'tmx', 'tmp', 'pet')
datasets <- c('tmp')

# This is the projection of the CRU files
s_srs <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'

ISO_2s <- c("ID", "ET", "UG", "NE", "ER")

foreach (dataset=datasets, .inorder=FALSE,
         .packages=c("teamlucc", "rgeos", "raster", "rgdal")) %dopar% {
    timestamp()
    message('Processing ', dataset, '...')

    ncdf <- file.path(in_folder, dataset,
                      pattern=paste(product, datestring, dataset, 'dat.nc', 
                                    sep='.'))
    this_dataset <- stack(ncdf)
    proj4string(this_dataset) <- s_srs

    for (ISO_2 in ISO_2s) {
        ISO_3 <- as.character(iso_key$ISO_3[match(ISO_2, iso_key$ISO_2)])
        aoi <- readOGR(shp_folder, paste0(ISO_3, '_adm0'))
        stopifnot(length(aoi) == 1)
        aoi <- gConvexHull(aoi)
        aoi <- spTransform(aoi, CRS(utm_zone(aoi, proj4string=TRUE)))
        aoi <- gBuffer(aoi, width=100000)
        aoi <- spTransform(aoi, CRS(s_srs))

        dstfile <- file.path(out_folder,
                              paste0(ISO_2, "_", product, '_', dataset, '_', 
                                     datestring,  '.tif'))
        cropped_data <- crop(this_dataset, aoi, overwrite=TRUE, filename=dstfile)
    }
}

stopCluster(cl)
