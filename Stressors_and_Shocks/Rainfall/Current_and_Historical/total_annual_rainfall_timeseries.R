##############################################################################
# Crops CHIRPS pentad or monthly precipitation data to cover the spatial extent 
# of each site.
###############################################################################

prefix <- "O:/Data"

#library(cartodbr)
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

#dataset <- 'pentad'
dataset <- 'monthly'

# Over what period should the calculations be made?
mean_monthly_period <- '198501-201412'

in_folder <- file.path(prefix, "CHIRPS-2.0", paste0('global-', dataset))
out_folder <- file.path(prefix, "GRP", "CHIRPS-2.0")
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

period_start <- str_extract(mean_monthly_period, '^[0-9]*(?=-)')
period_start_date <- as.Date(paste0(period_start, '01'), '%Y%m%d')
period_end <- str_extract(mean_monthly_period, '(?<=-)[0-9]*')
period_end_date <- as.Date(paste0(period_end, '01'), '%Y%m%d')
period_dates <- seq(period_start_date, period_end_date, by='month')

base_name <- file.path(out_folder, paste0('CHIRPS_',
                                          period_start, '-', period_end))

# Build a VRT with all dates in a single layer stacked VRT file (this stacks 
# the tifs, but with delayed computation - the actual cropping and stacking 
# computations won't take place until the gdalwarp line below that is run for 
# each aoi)
vrt_file <- extension(rasterTmpFile(), 'vrt')
gdalbuildvrt(file.path(in_folder, tifs[which(file_dates %in% period_dates)]), 
             vrt_file, separate=TRUE, overwrite=TRUE)
chirps <- stack(vrt_file)

n_years <- floor(nlayers(chirps) / 12)
annual_total <- foreach(n=1:n_years, .combine=raster::stack, 
                        .packages='raster') %dopar% {
    start_layer <- 1 + (n - 1) * 12
    end_layer <- n * 12
    total <- sum(chirps[[start_layer:end_layer]])
    total[total < 0] <- -9999
    total
}
annual_total <- writeRaster(annual_total,
                            filename=paste0(base_name, '_annualtotal.tif'), 
                            overwrite=TRUE)
