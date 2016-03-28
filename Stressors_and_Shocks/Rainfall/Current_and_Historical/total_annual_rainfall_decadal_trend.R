##############################################################################
# Crops CHIRPS pentad or monthly precipitation data to cover the spatial extent 
# of each site.
###############################################################################

prefix <- "O:/Data"
prefix <- "/localdisk/home/azvoleff/Data"

library(raster)
library(lubridate)
library(tools)
library(foreach)
library(doParallel)
library(spatial.tools)

cl  <- makeCluster(28)
registerDoParallel(cl)

ppt_annual_ts_file <- file.path(prefix, "GRP", "CHIRPS-2.0", "CHIRPS_198501-201412_annualtotal.tif")
dates <- seq(as.Date('1985/1/1'), as.Date('2014/1/1'), 'year')
mean_annual_ppt_file <- file.path(prefix, "CHPclim", "total_annual_rainfall.tif")
out_folder <- file.path(prefix, "GRP", "CHIRPS-2.0")
stopifnot(file_test('-f', ppt_annual_ts_file))
stopifnot(file_test('-d', out_folder))

base_name <- file_path_sans_ext(ppt_annual_ts_file)

ppt_annual_ts <- stack(ppt_annual_ts_file)
mean_annual_ppt <- raster(mean_annual_ppt_file)

# Function to calculate trend
calc_decadal_trend <- function(ppt_annual_ts, mean_annual_ppt, dates, ...) {
    browser()
    dims <- dim(ppt_annual_ts)
    mean_annual_ppt[mean_annual_ppt == -9999] <- NA
    # Setup period identifiers so the data can be used in a dataframe
    years <- year(dates)
    years_rep <- rep(years, each=dims[1]*dims[2])
    mean_annual_ppt_rep <- rep(mean_annual_ppt, each=dims[3])
    pixels_rep <- rep(seq(1:(dims[1]*dims[2])), dims[3])
    p_df <- data.frame(year=years_rep,
                       pixel=pixels_rep,
                       ppt_pct_mean=(as.vector(ppt_annual_ts)/mean_annual_ppt_rep)*100)
    # Map areas that are getting signif. wetter or drier, coded by mm per year
    extract_coefs <- function(indata) {
        if (sum(!is.na(indata$ppt_pct_means)) < 3) {
            d <- data.frame(coef=c('(Intercept)', 'year'), c(NA, NA), c(NA, NA))
        } else {
            model <- lm(ppt_pct_mean ~ year, data=indata)
            d <- data.frame(summary(model)$coefficients[, c(1, 4)])
            d <- cbind(row.names(d), d)
        }
        names(d) <- c('coef', 'estimate', 'p_val')
        row.names(d) <- NULL
        return(d)
    }
    lm_coefs <- group_by(p_df, pixel) %>%
        # Note below divides by CHPclim annual mean, not CHIRPS climatology
        do(extract_coefs(.))
    # Note the *10 below to convert to decadal change
    out <- array(c(filter(lm_coefs, coef == "year")$estimate * 10,
                   filter(lm_coefs, coef == "year")$p_val),
                 dim=c(dims[1], dims[2], 2))
    # Mask out nodata areas
    out[ , , 1][is.na(ppt_annual_ts[ , , 1])] <- NA
    out[ , , 2][is.na(ppt_annual_ts[ , , 1])] <- NA
    out
}

print('Started calculating decadal trend in total annual precip timeseries...')
timestamp()
decadal_trend <- rasterEngine(ppt_annual_ts=ppt_annual_ts,
    mean_annual_ppt=mean_annual_ppt, args=list(dates=dates),
    fun=calc_decadal_trend, datatype='FLT4S', outbands=2, outfiles=1,
    processing_unit="chunk", filename=paste0(base_name, '_trend_decadal'),
    .packages=c('dplyr', 'lubridate'), overwrite=TRUE)
writeRaster(decadal_trend,
            filename=paste0(base_name, '_trend_decadal_geotiff.tif'),
            overwrite=TRUE)
timestamp()
print('Finished calculating decadal trend in total annual precip timeseries.')

stopCluster(cl)
