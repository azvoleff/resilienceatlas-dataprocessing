###############################################################################
# Extracts precipitation timeseries for specific points.
###############################################################################

library(raster)
library(stringr)
library(reshape2)
library(lubridate)

library(doParallel)
library(foreach)

n_cpus <- 4

overwrite <- TRUE

product <- 'v1p8chirps'
chirps_NA_value <- -9999
# dataset <- 'pentad'
# date_limits_string <- '198101-201424'
dataset <- 'monthly' # For SPI, use monthly
date_limits_string <- '198101-201404'
# Note the below code is INCLUSIVE of the start date
chirps_start_date <- as.Date('1981/1/1')
# Note the below code is EXCLUSIVE of the end date
chirps_end_date <- as.Date('2014/5/1')

in_folder <- 'O:/Data/Vital Signs/CHIRPS_Data'
out_folder <- 'O:/Data/Vital Signs/CHIRPS_Data'
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))

if (dataset == 'monthly') {
    dates <- seq(chirps_start_date, chirps_end_date, by='months')
    dates <- dates[dates < chirps_end_date]
    num_periods <- 12
} else if (dataset == 'pentad') {
    yrs <- seq(year(chirps_start_date), year(chirps_end_date))
    # Have 72 pentads per year (12 months per year, 6 pentads per month)
    yrs_rep <- rep(yrs, each=12*6)
    days <- c(1, 6, 11, 16, 21, 26)
    mths <- rep(paste(rep(seq(1, 12), each=6), days, sep='/'), length(yrs))
    dates <- as.Date(paste(yrs_rep, mths, sep='/'))
    dates <- dates[dates < chirps_end_date]
    num_periods <- 72
}

# This is the projection of the CHIRPS files, read from the .hdr files 
# accompanying the data
s_srs <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'

###############################################################################
###############################################################################
# First extract the precipitation timeseries
###############################################################################
###############################################################################

###############################################################################
# Load CHIRPS data
chirps_file_masked <- file.path(in_folder,
                      paste0(product, '_', dataset, '_VitalSigns_', 
                             date_limits_string, '_NAsmasked.envi'))
chirps <- brick(chirps_file_masked)

###############################################################################
# First extract timeseries for LSMS households
lsms_hhs <- readOGR("O:/Data/Vital Signs/LSMS_Data", "TZ_HH_pt_locations")
lsms_hhs <- spTransform(lsms_hhs, CRS(s_srs))

beginCluster(n_cpus)
hh_ppt <- extract(chirps, lsms_hhs, df=TRUE)
endCluster()
hh_ppt <- hh_ppt[names(hh_ppt) != 'ID']
hh_ppt <- cbind(DISTRICT=lsms_hhs$DISTRICT,
                WARD=lsms_hhs$WARD, 
                EA=lsms_hhs$EA, 
                HHNO=lsms_hhs$HHNO, 
                CYCLE=lsms_hhs$CYCLE, 
                HHID=lsms_hhs$HHID, 
                hh_ppt)
hh_ppt <- melt(hh_ppt, id.vars=c("DISTRICT", "WARD", "EA", "HHNO", "CYCLE", "HHID"),
               variable.name='date', value.name='ppt')
hh_ppt$date <- as.numeric(str_extract(hh_ppt$date, '[0-9]*$'))
if (dataset == 'pentad') {
    hh_ppt$pentad <- hh_ppt$date %% num_periods
}
hh_ppt$date <- dates[hh_ppt$date]
hh_ppt$year <- year(hh_ppt$date)
hh_ppt$month <- month(hh_ppt$date)
save(hh_ppt, file=paste0(dataset, '_hh_ppt.RData'))

###############################################################################
# Now extract timeseries averaged over the Vital Signs Landscapes:
vs_landscapes <- readOGR("O:/Data/Vital Signs/VS_Landscapes", "VS_LANDSCAPES")
vs_landscapes <- spTransform(vs_landscapes, CRS(s_srs))

# Filter out Vital Signs landscapes in Ghana
vs_landscapes <- vs_landscapes[!grepl('GHA', vs_landscapes$New_ID), ]

vsls_ppt <- extract(chirps, vs_landscapes)
stopifnot(length(vsls_ppt) == length(vs_landscapes))
# Extract from a polygon returns a list of length equal to number of polygons.  
# So need to convert this list to a data.frame
vsls_ppt <- foreach(n=1:length(vsls_ppt), .combine=rbind) %do% {
    this_vsls_ppt <- data.frame(vsls_ppt[n])
    this_vsls_ppt <- cbind(ID=vs_landscapes$New_ID[n], this_vsls_ppt)
    melt(this_vsls_ppt, id.vars=c("ID"), variable.name='date', value.name='ppt')
}
vsls_ppt$date <- as.numeric(str_extract(vsls_ppt$date, '[0-9]*$'))
if (dataset == 'pentad') {
    vsls_ppt$pentad <- vsls_ppt$date %% num_periods
}
vsls_ppt$date <- dates[vsls_ppt$date]
vsls_ppt$year <- year(vsls_ppt$date)
vsls_ppt$month <- month(vsls_ppt$date)
save(vsls_ppt, file=paste0(dataset, '_VSLandscapes_ppt.RData'))

###############################################################################
###############################################################################
# Now extract the SPI timeseries
###############################################################################
###############################################################################
###############################################################################

# Load SPI data
spi_file_masked <- file.path(in_folder, 'monthly_VitalSigns_SPI_24.envi')
spi <- brick(spi_file_masked)

###############################################################################
# First extract timeseries for LSMS households
lsms_hhs <- readOGR("O:/Data/Vital Signs/LSMS_Data", "TZ_HH_pt_locations")
lsms_hhs <- spTransform(lsms_hhs, CRS(s_srs))

beginCluster(n_cpus)
hh_spi <- extract(spi, lsms_hhs, df=TRUE)
endCluster()
hh_spi <- hh_spi[names(hh_spi) != 'ID']
hh_spi <- cbind(DISTRICT=lsms_hhs$DISTRICT,
                WARD=lsms_hhs$WARD, 
                EA=lsms_hhs$EA, 
                HHNO=lsms_hhs$HHNO, 
                CYCLE=lsms_hhs$CYCLE, 
                HHID=lsms_hhs$HHID, 
                hh_spi)
hh_spi <- melt(hh_spi, id.vars=c("DISTRICT", "WARD", "EA", "HHNO", "CYCLE", "HHID"),
               variable.name='date', value.name='SPI')
hh_spi$date <- as.numeric(str_extract(hh_spi$date, '[0-9]*$'))
hh_spi$date <- dates[hh_spi$date]
hh_spi$year <- year(hh_spi$date)
hh_spi$month <- month(hh_spi$date)
save(hh_spi, file='hh_spi24.RData')

###############################################################################
# Now extract timeseries averaged over the Vital Signs Landscapes:
vs_landscapes <- readOGR("O:/Data/Vital Signs/VS_Landscapes", "VS_LANDSCAPES")
vs_landscapes <- spTransform(vs_landscapes, CRS(s_srs))

# Filter out Vital Signs landscapes in Ghana
vs_landscapes <- vs_landscapes[!grepl('GHA', vs_landscapes$New_ID), ]

vsls_spi <- extract(spi, vs_landscapes)
stopifnot(length(vsls_spi) == length(vs_landscapes))
# Extract from a polygon returns a list of length equal to number of polygons.  
# So need to convert this list to a data.frame
vsls_spi <- foreach(n=1:length(vsls_spi), .combine=rbind) %do% {
    this_vsls_spi <- data.frame(vsls_spi[n])
    this_vsls_spi <- cbind(ID=vs_landscapes$New_ID[n], this_vsls_spi)
    melt(this_vsls_spi, id.vars=c("ID"), variable.name='date', value.name='SPI')
}
vsls_spi$date <- as.numeric(str_extract(vsls_spi$date, '[0-9]*$'))
vsls_spi$date <- dates[vsls_spi$date]
vsls_spi$year <- year(vsls_spi$date)
vsls_spi$month <- month(vsls_spi$date)
save(vsls_spi, file='VSLandscapes_spi24.RData')
