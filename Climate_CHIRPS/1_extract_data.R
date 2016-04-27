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

warp_threads <- 3

#dataset <- 'pentad'
dataset <- 'monthly'

period_included <- '198501-201412'

in_folder <- file.path(prefix, "CHIRPS-2.0", paste0('global-', dataset))
# out_folder <- file.path(prefix, "GRP", "CHIRPS-2.0")
# shp_folder <- file.path(prefix, "GRP", "Boundaries")
out_folder <- file.path(prefix, "Vital_Signs", "CHIRPS-2.0")
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

datestrings <- gsub('[.]', '', datestrings)
start_date <- datestrings[1]
end_date <- datestrings[length(datestrings)]
file_dates <- as.Date(paste0(datestrings, '01'), '%Y%m%d')

period_start <- str_extract(period_included, '^[0-9]*(?=-)')
period_start_date <- as.Date(paste0(period_start, '01'), '%Y%m%d')
period_end <- str_extract(period_included, '(?<=-)[0-9]*')
period_end_date <- as.Date(paste0(period_end, '01'), '%Y%m%d')
period_dates <- seq(period_start_date, period_end_date, by='month')

base_name <- file.path(out_folder, paste0('CHIRPS_', dataset, '_', 
                                          period_start, '-', period_end))
out_tif <- paste0(base_name, '.tif')

# Build a VRT with all dates in a single layer stacked VRT file (this stacks 
# the tifs, but with delayed computation - the actual cropping and stacking 
# computations won't take place until the gdalwarp line below that is run for 
# each aoi)
vrt_file <- extension(rasterTmpFile(), 'vrt')
gdalbuildvrt(file.path(in_folder, tifs[which(file_dates %in% period_dates)]), 
             vrt_file, separate=TRUE, overwrite=TRUE)

# This is the projection of the CHIRPS files, read from the .hdr files 
# accompanying the data
s_srs <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'

print('Starting')
timestamp()

chirps <- gdalwarp(vrt_file, out_tif, s_srs=s_srs, multi=TRUE, 
                   wo=paste0("NUM_THREADS=", warp_threads), overwrite=TRUE, 
                   output_Raster=TRUE)
print('Finished')
timestamp()
