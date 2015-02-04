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
library(teamlucc)
library(foreach)
library(doParallel)

n_cpus <- 3

cl  <- makeCluster(n_cpus)
registerDoParallel(cl)

#dataset <- 'pentad'
dataset <- 'monthly'

in_folder <- file.path('J:/CHIRPS_Originals', paste0('global_', dataset))
out_folder <- file.path(prefix, "GRP", "CHIRPS")
shp_folder <- file.path(prefix, "GRP", "Boundaries", "Regional")
stopifnot(file_test("-d", shp_folder))
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))

tifs <- dir(in_folder, pattern='.tif$')

datestrings <- gsub('.tif', '', (str_extract(tifs, '[0-9]{4}\\.[0-9]{2}.tif$')))
years <- as.numeric(str_extract(datestrings, '^[0-9]{4}'))
# The subyears strings are numeric codes referring to either pentads or months, 
# depending on the dataset chosen.
subyears <- as.numeric(str_extract(datestrings, '[0-9]{2}$'))

datestrings <- datestrings[order(years, subyears)]
tifs <- tifs[order(years, subyears)]

product <- unique(str_extract(tifs, '^v[0-9]*p[0-9]*chirps'))
stopifnot(length(product) == 1)

# Build a VRT with all dates in a single layer stacked VRT file (this stacks 
# the tifs, but with delayed computation - the actual cropping and stacking 
# computations won't take place until the gdalwarp line below that is run for 
# each aoi)
vrt_file <- extension(rasterTmpFile(), 'vrt')
gdalbuildvrt(file.path(in_folder, '*.tif'), vrt_file, separate=TRUE, 
             overwrite=TRUE)

# This is the projection of the CHIRPS files, read from the .hdr files 
# accompanying the data
s_srs <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'

region_polygons <- readOGR(shp_folder, 'GRP_regions')

foreach (n=c(1:nrow(region_polygons)), .inorder=FALSE,
         .packages=c('raster', 'teamlucc', 'rgeos', 'gdalUtils',
                     'rgdal')) %dopar% {
    timestamp()

    aoi <- region_polygons[n, ]
    region <- as.character(aoi$Region)
    region <- gsub(' ', '', region)
    aoi <- gConvexHull(aoi)
    aoi <- spTransform(aoi, CRS(utm_zone(aoi, proj4string=TRUE)))
    aoi <- gBuffer(aoi, width=100000)
    aoi <- spTransform(aoi, CRS(s_srs))
    te <- as.numeric(bbox(aoi))

    chirps_tif <- file.path(out_folder,
                            paste0(region, '_', product, '_', dataset, '_', 
                                   datestrings[1], '-', 
                                   datestrings[length(datestrings)], '.tif'))
    # Crop tifs for this site
    gdalwarp(vrt_file, chirps_tif, s_srs=s_srs, t_srs=s_srs, te=te, 
             multi=TRUE, wo=paste0("NUM_THREADS=", n_cpus), 
             overwrite=TRUE)

    chirps <- brick(chirps_tif)

    chirps_NA_value <- -9999
    chirps_tif_masked <- file.path(out_folder,
                            paste0(region, '_', product, '_', dataset, '_', 
                                   datestrings[1], '-', 
                                   datestrings[length(datestrings)], '_NAs_masked.tif'))
    chirps <- calc(chirps, function(vals) {
            vals[vals == chirps_NA_value] <- NA
            return(vals)
        }, filename=chirps_tif_masked, overwrite=TRUE)
}
