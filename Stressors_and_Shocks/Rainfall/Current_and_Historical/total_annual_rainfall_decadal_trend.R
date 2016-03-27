###############################################################################
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
# What periods should anomalies be calculated over?

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
    sum(chirps[[start_layer:end_layer]])
}
annual_total <- writeRaster(annual_total,
                            filename=paste0(base_name, '_annualtotal.tif'), 
                            overwrite=TRUE)

coef_var <- cv(annual_total)
writeRaster(coef_var,
            filename=paste0(base_name, '_interannualvariability_pct.tif'),
            overwrite=TRUE)

print('Finished calculating annual total precip timeseries.')

print('Calculating decadal trend in total annual precip timeseries...')
timestamp()

# Function to calculate trend
calc_decadal_trend <- function(p, dates, included_subyears, ...) {
    p[p == -9999] <- NA
    # Setup period identifiers so the data can be used in a dataframe
    years <- year(dates)
    years_rep <- rep(years, each=dim(p)[1]*dim(p)[2])
    subyears <- rep(seq(1, 12),  length.out=dim(p)[3])
    subyears_rep <- rep(subyears, each=dim(p)[1]*dim(p)[2])
    pixels_rep <- rep(seq(1:(dim(p)[1]*dim(p)[2])), dim(p)[3])
    p_df <- data.frame(year=years_rep,
                       subyear=subyears_rep, 
                       pixel=pixels_rep,
                       ppt=as.vector(p))
    if (!is.na(included_subyears)) {
        p_df <- dplyr::filter(p_df, subyear %in% included_subyears)
    }
    # Map areas that are getting signif. wetter or drier, coded by mm per 
    # year
    extract_coefs <- function(indata) {
        if (sum(!is.na(indata$ppt_annual_pctmean)) < 3) {
            d <- data.frame(coef=c('(Intercept)', 'year'), c(NA, NA), c(NA, NA))
        } else {
            model <- lm(ppt_annual_pctmean ~ year, data=indata)
            d <- data.frame(summary(model)$coefficients[, c(1, 4)])
            d <- cbind(row.names(d), d)
        }
        names(d) <- c('coef', 'estimate', 'p_val')
        row.names(d) <- NULL
        return(d)
    }
    lm_coefs <- group_by(p_df, year, pixel) %>%
        summarize(ppt_annual=sum(ppt, na.rm=TRUE)) %>%
        group_by(pixel) %>%
        # TODO: below line should divide by CHPclim annual mean, not CHIRPs 
        # climatology
        mutate(ppt_annual_pctmean=(ppt_annual/mean(ppt_annual))*100) %>%
        do(extract_coefs(.))
    # Note the *10 below to convert to decadal change
    out <- array(c(filter(lm_coefs, coef == "year")$estimate * 10,
                   filter(lm_coefs, coef == "year")$p_val),
                 dim=c(dim(p)[1], dim(p)[2], 2))
    # Mask out nodata areas
    out[ , , 1][is.na(p[ , , 1])] <- NA
    out[ , , 2][is.na(p[ , , 1])] <- NA
    out
}
decadal_trend <- rasterEngine(p=chirps,
    args=list(dates=dates, included_subyears=included_subyears),
    fun=calc_decadal_trend, datatype='FLT4S', outbands=2, outfiles=1, 
    processing_unit="chunk",
    filename=paste0(base_name, '_trend_decadal', season_string),
    .packages=c('dplyr', 'lubridate'), overwrite=TRUE)
writeRaster(decadal_trend,
            filename=paste0(base_name, '_trend_decadal', season_string, '_geotiff.tif'),
            overwrite=TRUE)

}
timestamp()
print('Finished calculating decadal trend in total annual precip timeseries...')

stopCluster(cl)
