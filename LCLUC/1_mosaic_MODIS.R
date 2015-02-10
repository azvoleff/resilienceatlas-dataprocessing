###############################################################################
# Mosaics MODIS MCD12Q1 tiles (Land Cover Type) products to cover the spatial 
# extent of the ZOI/CSA/PA boundary of each team site.
# 
# Requires a GDAL installation that supports HDF4 files - on Windows, see 
# OSGEO4W to meet this dependency.
###############################################################################

source('../0_settings.R')

library(rgdal)
library(raster)
library(stringr)
library(gdalUtils)
library(rgeos)
library(teamlucc)

library(doParallel)
library(foreach)

n_cpus <- 4

registerDoParallel(n_cpus)

in_folder <- file.path("D:/MODIS_Originals", "MCD12Q1")
out_folder <- file.path(prefix, "GRP", "MODIS", 'MCD12Q1')

stopifnot(file_test("-d", in_folder))
stopifnot(file_test("-d", out_folder))

tile_key <- read.csv('GRP_MODIS_key.csv')

# hardcode aoi for now:
aois <- data.frame(sitename="Niger",
                   path=file.path(prefix, 'GRP', 'Boundaries', 'National'),
                   layer="NER_adm0", stringsAsFactors=FALSE)

sitename <- unique(tile_key$sitename)[1]

for (sitename in unique(tile_key$sitename)) {
    timestamp()
    message('Processing ', sitename, '...')
    site_rows <- tile_key[tile_key$sitename == sitename, ]
    tile_ids <- paste0('h', sprintf('%02i', site_rows$h),
                       'v', sprintf('%02i', site_rows$v))

    aoi_row <- which(aois$sitename == sitename)

    aoi <- readOGR(aois$path[aoi_row], aois$layer[aoi_row])
    aoi <- gConvexHull(aoi)
    aoi <- spTransform(aoi, CRS(utm_zone(aoi, proj4string=TRUE)))
    aoi <- gBuffer(aoi, width=100000)
    aoi <- spTransform(aoi, CRS("+init=epsg:4326"))

    t_srs <- proj4string(aoi)
    te <- as.numeric(bbox(aoi))

    tile_regex <- paste(paste0('(', tile_ids, ')'), collapse='|')
    hdfs <- dir(in_folder, pattern='.hdf$')
    tiles <- hdfs[grepl(tile_regex, hdfs)]
    if (length(tiles) == 0) {
        stop('no tiles found')
    }
    product <- gsub('[.]', '', str_extract(tiles, '^[a-zA-Z0-9]*[.]'))
    if (length(unique(product)) != 1) {
        stop('tiles are from more than one MODIS product')
    }
    product <- product[1]

    dates <- unique(as.Date(str_extract(tiles, '[0-9]{7}'), '%Y%j'))

    foreach(this_date=iter(dates),
            .packages=c('raster', 'gdalUtils', 'stringr'), 
            .inorder=FALSE) %dopar% {
        message(this_date)
        tiles_by_date <- tiles[grepl(format(this_date, '%Y%j'), tiles)]
        srcfiles <- file.path(in_folder, tiles_by_date)
        out_base <- file.path(out_folder,
                              paste(product, gsub(' ', '', sitename),
                                    format(this_date, '%Y%j'), sep='_'))

        subdatasets <- get_subdatasets(srcfiles[[1]])
        band_names <- data.frame(band=seq(1, length(subdatasets)),
                                 name=gsub(':', '', str_extract(subdatasets, ':[a-zA-Z0-9_]*$')))
        write.csv(band_names, file=paste0(out_base, '_bandnames.csv'), row.names=FALSE)

        # First build a VRT with all the bands in the HDF file (this mosaics 
        # the tiles, but with delayed computation - the actual mosaicing 
        # computations won't take place until the gdalwarp line below)
        vrt_files <- c()
        for (n in 1:length(srcfiles)) {
            vrt_file <- extension(rasterTmpFile(), 'vrt')
            gdalbuildvrt(srcfiles[[n]], vrt_file, separate=TRUE)
            vrt_files <- c(vrt_files, vrt_file)
        }

        # Mosaic, reproject, and crop vrts
        dstfile <- paste0(out_base, '.tif')
        # Note that tr in the below gives pixel sizes of one degree
        gdalwarp(vrt_files, dstfile, t_srs=t_srs, te=te,
                 tr=c(0.00416667, 0.00416667), 
                 tap=TRUE, multi=TRUE, wo=paste0("NUM_THREADS=", n_cpus), 
                 overwrite=TRUE)
    }

}
