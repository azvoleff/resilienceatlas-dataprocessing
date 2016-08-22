###############################################################################
# Crops CHIRPS pentad or monthly precipitation data to cover the spatial extent 
# of each site.
###############################################################################

source('../0_settings.R')

library(rgdal)
library(raster)
library(stringr)
library(gdalUtils)
library(rgeos)
library(foreach)
library(spatial.tools)
library(doParallel)

cl  <- makeCluster(4)
registerDoParallel(cl)

in_folder <- file.path(prefix, "WorldClim")
out_folder <- file.path(prefix, "Resilience_Atlas", "WorldClim")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))

rwa <- readOGR(file.path(prefix, 'Global', 'GADM'), 'RWA_adm0')

datasets <- c('prec', 'tmax', 'tmin', 'tmean')

foreach(dataset=datasets,
        .packages=c('raster', 'stringr', 'gdalUtils', 'rgeos')) %dopar% {
    base_name <- file.path(out_folder, paste0(dataset, '_Rwanda'))

    bils <- dir(in_folder, pattern=paste0(dataset, '_1?[0-9].bil$'))
    months <- as.numeric(str_extract(bils, '1?[0-9]'))
    vrt_file <- extension(rasterTmpFile(), 'vrt')
    gdalbuildvrt(file.path(in_folder, bils[order(months)]), vrt_file, 
                           separate=TRUE, overwrite=TRUE)

    aoi <- gConvexHull(rwa)
    t_srs <- proj4string(aoi)
    te <- as.numeric(bbox(aoi))
    d <- gdalwarp(vrt_file, extension(rasterTmpFile(), 'tif'), t_srs=t_srs, 
                  te=te, overwrite=TRUE, ot="Float32", output_Raster=TRUE)

    if (dataset == 'prec') {
        d <- sum(d)
        d <- mask(d, rwa)
        writeRaster(d, filename=paste0(base_name, '_annual_sum.tif'), 
                    overwrite=TRUE)
    } else {
        d <- mean(d)/10
        d <- mask(d, rwa)
        writeRaster(d, filename=paste0(base_name, '_annual_mean.tif'), 
                    overwrite=TRUE)
    }
}
