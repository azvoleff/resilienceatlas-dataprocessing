##############################################################################
# Crops CHIRPS pentad or monthly precipitation data to cover the spatial extent 
# of each site.
###############################################################################

#prefix <- "H:/Data"
prefix <- "/localdisk/home/azvoleff/Data"

library(raster)
library(maptools) # For testing
library(broom)
library(dplyr)
library(lubridate)
library(tools)
library(foreach)
library(doParallel)
library(spatial.tools)

cl  <- makeCluster(28)
registerDoParallel(cl)

ppt_annual_ts_file <- file.path(prefix, "Resilience_Atlas", "CHIRPS-2.0", "CHIRPS_198501-201412_annualtotal.tif")
dates <- seq(as.Date('1985/1/1'), as.Date('2014/1/1'), 'year')
mean_annual_ppt_file <- file.path(prefix, "CHPclim", "total_annual_rainfall.tif")
out_folder <- file.path(prefix, "Resilience_Atlas", "CHIRPS-2.0")
stopifnot(file_test('-f', ppt_annual_ts_file))
stopifnot(file_test('-d', out_folder))

base_name <- file_path_sans_ext(ppt_annual_ts_file)

ppt_annual_ts <- stack(ppt_annual_ts_file)
mean_annual_ppt <- raster(mean_annual_ppt_file)

### For testing
# aoi <- readShapeSpatial('H:/Data/Global/GADM/TZA_adm0.shp')
# ppt_annual_ts <- crop(ppt_annual_ts, aoi)
# mean_annual_ppt <- crop(mean_annual_ppt, aoi)
### /For testing

# Function to calculate precipitation trend, to map areas that are getting 
# signif.  wetter or drier, coded by mm per decade
calc_decadal_trend <- function(ppt_annual_ts, mean_annual_ppt, dates, ...) {
    dims <- dim(ppt_annual_ts)
    mean_annual_ppt[mean_annual_ppt < 0] <- NA
    ppt_annual_ts[ppt_annual_ts < 0] <- NA
    # Setup period identifiers so the data can be used in a dataframe
    years <- year(dates)
    years_rep <- rep(years, each=dims[1]*dims[2])
    mean_annual_ppt_rep <- rep(mean_annual_ppt, dims[3])
    pixels_rep <- rep(seq(1:(dims[1]*dims[2])), dims[3])
    p_df <- data.frame(year=years_rep,
                       pixel=pixels_rep,
                       ppt_annual_ts=as.vector(ppt_annual_ts),
                       mean_annual_ppt=as.vector(mean_annual_ppt_rep),
                       ppt_pct_mean=(as.vector(ppt_annual_ts)/mean_annual_ppt_rep)*100)
    model_trend <- function(indata) {
        if (all(is.na(indata)) | all(is.infinite(unlist(indata))) | 
            all(is.nan(unlist(indata)))) {
            return(data.frame(estimate=NA, p.value=NA))
        } else {
            model <- lm(ppt_pct_mean ~ year, data=indata)
            return(tidy(model) %>%
                   filter(term == 'year') %>%
                   select(estimate, p.value))
        }
    }
    lm_estimates <- group_by(p_df, pixel) %>%
        do(model_trend(.))
    # Note the *10 below to convert to decadal change
    out <- array(c(lm_estimates$estimate * 10,
                   lm_estimates$p.value),
                 dim=c(dims[1], dims[2], 2))
    # Code nodata areas as -9999
    out[ , , 1][is.na(ppt_annual_ts[ , , 1])] <- -9999
    out[ , , 2][is.na(ppt_annual_ts[ , , 1])] <- -9999
    out[is.na(out)] <- -9999
    out
}

print('Started calculating decadal trend in total annual precip timeseries...')
timestamp()
decadal_trend <- rasterEngine(ppt_annual_ts=ppt_annual_ts,
    mean_annual_ppt=mean_annual_ppt, args=list(dates=dates),
    fun=calc_decadal_trend, datatype='FLT4S', outbands=2, outfiles=1,
    processing_unit="chunk", filename=paste0(base_name, '_trend_decadal'),
    .packages=c('dplyr', 'lubridate', 'broom'), overwrite=TRUE)
writeRaster(decadal_trend,
            filename=paste0(base_name, '_trend_decadal_geotiff.tif'),
            overwrite=TRUE)

timestamp()
print('Finished calculating decadal trend in total annual precip timeseries.')

stopCluster(cl)

#plot(decadal_trend[[1]], zlim=c(-30, 30))
