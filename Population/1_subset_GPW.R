###############################################################################
# Crops GPW population data to cover the spatial extent of the ZOI/CSA/PA 
# boundary of each team site.
###############################################################################

source('../0_settings.R')

library(rgdal)
library(raster)
library(stringr)
library(gdalUtils)
library(rgeos)
library(teamlucc)

pop_folder <- file.path(prefix, "GPWv4", "tifs")
shp_folder <- file.path(prefix, "GRP", "Boundaries", "National")
out_folder <- file.path(prefix, "GRP", "GPWv4")
stopifnot(file_test("-d", pop_folder))
stopifnot(file_test("-d", shp_folder))
stopifnot(file_test("-d", out_folder))

iso_key <- read.csv(file.path(prefix, "Global", "ISO_Codes.csv"))

# CNTM is count
count_name <- "CNTM"
count_regex <- "GL_E_ATOTPOPBT_[0-9]{4}_CNTM"

# DENS is density in persons per square km
dens_name <- "DENS"
dens_regex <- "GL_E_ATOTPOPBT_[0-9]{4}_DENS"

# AREAKM is grid cell area in square km
areakm_name <- "AREAKM"
areakm_regex <- "GL_AREAKM"

dens_tifs <- dir(pop_folder, pattern=paste0(dens_regex, ".tif$"))
dens_years <- str_extract(dens_tifs, '[0-9]{4}')
dens_tifs <- dens_tifs[order(dens_years)]

count_tifs <- dir(pop_folder, pattern=paste0(count_regex, ".tif$"))
count_years <- str_extract(count_tifs, '[0-9]{4}')
count_tifs <- count_tifs[order(count_years)]

stopifnot(count_years == dens_years)
min_year <- min(count_years)
max_year <- max(count_years)

# Build a VRT with all dates in a single layer stacked VRT file (this stacks 
# the tifs, but with delayed computation - the actual cropping and stacking 
# computations won't take place until the gdalwarp line below that is run for 
# each aoi)
dens_vrt <- extension(rasterTmpFile(), 'vrt')
gdalbuildvrt(file.path(pop_folder, dens_tifs), dens_vrt, separate=TRUE, 
             overwrite=TRUE)

count_vrt <- extension(rasterTmpFile(), 'vrt')
gdalbuildvrt(file.path(pop_folder, count_tifs), count_vrt, separate=TRUE, 
             overwrite=TRUE)

areakm_orig <- file.path(pop_folder, dir(pop_folder, 
                                        pattern=paste0(areakm_regex, ".tif$")))

# This is the projection of the GPW files, as stated in the metadata file
s_srs <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'

ISO_2s <- c("ET", "DJ", "SO", "ER")

#ISO_2 <- ISO_2s[1]
for (ISO_2 in ISO_2s) {
    timestamp()

    ISO_3 <- as.character(iso_key$ISO_3[match(ISO_2, iso_key$ISO_2)])

    message('Processing ', ISO_3, '...')

    aoi <- readOGR(shp_folder, paste0(ISO_3, '_adm0'))
    stopifnot(length(aoi) == 1)

    aoi <- gConvexHull(aoi)
    aoi <- spTransform(aoi, CRS(utm_zone(aoi, proj4string=TRUE)))
    aoi <- gBuffer(aoi, width=100000)
    aoi <- spTransform(aoi, CRS(s_srs))
    te <- as.numeric(bbox(aoi))

    # Crop tifs for this site
    dens_tif <- file.path(out_folder, paste0(ISO_3, '_', dens_name, '_',
                                            min_year, '-', max_year, '.tif'))
    gdalwarp(dens_vrt, dens_tif, s_srs=s_srs, t_srs=s_srs, te=te, 
             multi=TRUE, wo=paste0("NUM_THREADS=", n_cpus), 
             overwrite=TRUE)

    count_tif <- file.path(out_folder, paste0(ISO_3, '_', count_name, '_',
                                              min_year, '-', max_year, '.tif'))
    gdalwarp(count_vrt, count_tif, s_srs=s_srs, t_srs=s_srs, te=te, 
             multi=TRUE, wo=paste0("NUM_THREADS=", n_cpus), 
             overwrite=TRUE)

    areakm_tif <- file.path(out_folder, paste0(ISO_3, '_', areakm_name, '.tif'))
    gdalwarp(areakm_orig, areakm_tif, s_srs=s_srs, t_srs=s_srs, te=te, 
             multi=TRUE, wo=paste0("NUM_THREADS=", n_cpus), 
             overwrite=TRUE)

}
