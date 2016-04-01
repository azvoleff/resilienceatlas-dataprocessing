#prefix <- "H:/Data"
prefix <- "/localdisk/home/azvoleff/Data"

library(maptools) # For testing
library(rgdal)
library(raster)
library(lubridate)
library(broom)
library(dplyr)
library(foreach)
library(doParallel)
library(spatial.tools)

cl  <- makeCluster(28)
registerDoParallel(cl)

product <- 'cru_ts3.23'
datestring <- '1901.2014'

# Note the below code is INCLUSIVE of the start date
cru_start_date <- as.Date('1901/1/1')
# Note the below code is EXCLUSIVE of the end date
cru_end_date <- as.Date('2015/1/1')

yrs <- seq(year(cru_start_date), year(cru_end_date))
dates <- seq(cru_start_date, cru_end_date, by='months')
dates <- dates[dates < cru_end_date]

# Choose a start and end year for the data to include in this analysis
start_date <- as.Date('1985/1/1') # Inclusive
end_date <- as.Date('2015/1/1') # Exclusive

new_datestring <- paste(format(start_date, '%Y%m%d'), format(end_date-1, '%Y%m%d'), sep='-')

#datasets <- c('tmn', 'tmx', 'tmp', 'pre')
datasets <- c('tmp')

# This is the projection of the CRU files
s_srs <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'

in_folder <- file.path(prefix, "CRU", product)
out_folder <- file.path(prefix, "Resilience_Atlas", "CRU")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))

foreach (dataset=datasets) %do% {
    print(paste('Started calculating decadal trend in CRU', dataset, 'timeseries...'))
    timestamp()
    filename_base <- file.path(out_folder,
                          paste0("Global", "_", product, '_', dataset, '_', 
                                 new_datestring))
    # Calculate the band numbers that are needed
    included_dates <- dates[(dates >= start_date) & (dates < end_date)]
    band_nums <- c(1:length(dates))[(dates >= start_date) & (dates < end_date)]

    ncdf <- file.path(in_folder, dataset,
                      pattern=paste(product, datestring, dataset, 'dat.nc', 
                                    sep='.'))
    cru_data <- stack(ncdf, bands=band_nums)
    proj4string(cru_data) <- s_srs

    ### For testing
    aoi <- readShapeSpatial('H:/Data/Global/GADM/TZA_adm0.shp')
    cru_data <- crop(cru_data, aoi)
    ### /For testing

    # Function to calculate precipitation trend, to map areas that are getting 
    # signif.  wetter or drier, coded by mm per decade
    calc_decadal_trend <- function(cru_data, dataset, dates, ...) {
        dims <- dim(cru_data)
        #TODO: Handle CRU NA codes
        #cru_data[cru_data < 0] <- NA
        # Setup period identifiers so the data can be used in a dataframe
        years <- year(dates)
        years_rep <- rep(years, each=dims[1]*dims[2])
        subyears <- rep(seq(1, 12),  length.out=dims[3])
        subyears_rep <- rep(subyears, each=dims[1]*dims[2])
        pixels_rep <- rep(seq(1:(dims[1]*dims[2])), dims[3])
        cru_data_df <- data.frame(year=years_rep,
                                  subyear=subyears_rep,
                                  pixel=pixels_rep,
                                  cru_data=as.vector(cru_data))
        model_trend <- function(indata) {
            tryCatch(
                {
                    model <- lm(annual_data ~ year, data=indata)
                    tidy(model) %>%
                        filter(term == 'year') %>%
                        select(estimate, p.value)
                }, error=function(cond) {
                    return(data.frame(estimate=NA, p.value=NA))
                }
            )
        }
        if (dataset == "pre") {
            lm_estimates <- group_by(cru_data_df, year, pixel) %>%
                summarize(ppt_annual=sum(cru_data, na.rm=TRUE)) %>%
                group_by(pixel) %>%
                mutate(annual_data=(ppt_annual/mean(ppt_annual))*100) %>%
                do(model_trend(.))
        } else {
            lm_estimates <- group_by(cru_data_df, year, pixel) %>%
                summarize(annual_data=mean(cru_data, na.rm=TRUE)) %>%
                group_by(pixel) %>%
                do(model_trend(.))
        }
        # Note the *10 below to convert to decadal change
        out <- array(c(lm_estimates$estimate * 10,
                       lm_estimates$p.value),
                     dim=c(dims[1], dims[2], 2))
        # Mask out nodata areas
        out[ , , 1][is.na(cru_data[ , , 1])] <- -9999
        out[ , , 2][is.na(cru_data[ , , 1])] <- -9999
        out[is.na(out)] <- -9999
        out
    }
    decadal_trend <- rasterEngine(cru_data=cru_data,
        args=list(dataset=dataset, dates=included_dates),
        fun=calc_decadal_trend, datatype='FLT4S', outbands=2, outfiles=1,
        processing_unit="chunk",
        filename=paste0(filename_base, '_trend_decadal_TEST'),
        .packages=c('dplyr', 'lubridate', 'broom'),
        overwrite=TRUE)
    writeRaster(decadal_trend,
                filename=paste0(filename_base, '_trend_decadal_TEST_geotiff.tif'),
                overwrite=TRUE)
    timestamp()
    print(paste('Finished calculating decadal trend in CRU', dataset, 'timeseries.'))
}
stopCluster(cl)
