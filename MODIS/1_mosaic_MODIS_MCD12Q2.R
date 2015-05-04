###############################################################################
# Mosaics MODIS MCD12Q2 (500m land cover dynamics product).
# 
# Requires a GDAL installation that supports HDF4 files - on Windows, see 
# OSGEO4W to meet this dependency.
###############################################################################

source('../0_settings.R')

n_cpus <- 3

library(rgdal)
library(raster)
library(stringr)
library(gdalUtils)
library(rgeos)
library(gfcanalysis) # for utm_zone
library(foreach)

modis_folder <- file.path(prefix, "GRP", "MODIS", "MCD12Q2")
out_folder <- file.path(prefix, "GRP", "MODIS", "MCD12Q2")
shp_folder <- file.path(prefix, "GRP", "Boundaries")
stopifnot(file_test("-d", modis_folder))
stopifnot(file_test("-d", out_folder))
stopifnot(file_test("-d", shp_folder))

aoi_polygons <- readOGR(shp_folder, 'Analysis_Areas')
aoi_polygons <- aoi_polygons[aoi_polygons$Name == "Horn of Africa", ]

aoi <- gConvexHull(aoi_polygons)
aoi <- spTransform(aoi, CRS(utm_zone(aoi, proj4string=TRUE)))
aoi <- gBuffer(aoi, width=5000)
aoi <- spTransform(aoi, CRS('+init=epsg:4326'))

t_srs <- proj4string(aoi)

# Round extent so that pixels are aligned properly
te <- as.numeric(bbox(aoi))
te[1:2] <- floor(te[1:2])
te[3:4] <- ceiling(te[3:4])

hdfs <- dir(modis_folder, pattern='.hdf$')

srcfiles <- file.path(modis_folder, hdfs)
out_base <- file.path(out_folder, paste('MCD12Q2_Horn_of_Africa'))

subdatasets <- get_subdatasets(srcfiles[[1]])
band_names <- data.frame(band=seq(1, length(subdatasets)),
                         name=gsub(':', '', str_extract(subdatasets, ':[a-zA-Z0-9_]*$')))
write.csv(band_names, file=paste0(out_base, '_bandnames.csv'), row.names=FALSE)

# First build a VRT with all the bands in the HDF file (this mosaics 
# the tiles, but with delayed computation - the actual mosaicing 
# computations won't take place until the gdalwarp line below)
vrt_file <- rasterTmpFile('.vrt')
gdalbuildvrt(srcfiles, vrt_file, overwrite=TRUE)

# Mosaic, reproject, and crop vrts
dstfile <- paste0(out_base, '.tif')
modis <- gdalwarp(vrt_file, dstfile, t_srs=t_srs, te=te,
                  tr=c(15/360, 15/360), r='cubicspline', multi=TRUE,
                  wo=paste0("NUM_THREADS=", n_cpus), overwrite=TRUE,
                  output_Raster=TRUE, verbose=TRUE)

# Delete the temp files
unlink(vrt_files)
