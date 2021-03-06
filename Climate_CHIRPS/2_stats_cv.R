###############################################################################
# Calculates coefficient of variation from 1985-2015 precip data
###############################################################################

source('../0_settings.R')

library(rgdal)
library(stringr)
library(raster)
library(tools)
library(dplyr)
library(foreach)
library(abind)
library(spatial.tools)
library(doParallel)
library(reshape2)

cl  <- makeCluster(3)
registerDoParallel(cl)

in_folder <- file.path(prefix, "GRP", "CHIRPS-2.0")
out_folder <- file.path(prefix, "GRP", "CHIRPS-2.0")
# in_folder <- file.path(prefix, "Vital_Signs", "CHIRPS-2.0")
# out_folder <- file.path(prefix, "Vital_Signs", "CHIRPS-2.0")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))

datafiles <- dir(in_folder, pattern='_CHIRPS_monthly_198101-201412.tif$')

# What periods should anomalies be calculated over?
anom_periods <- c(3, 6, 12)

# What periods should mean info be calculated over?
#comparison_periods <- c('198501-201412', '198501-199912', '200012-201412')
comparison_periods <- c('198501-201412')

print('Processing mean total monthly precips...')
foreach (datafile=datafiles) %:% 
    foreach(comparison_period=comparison_periods) %do% {

    timestamp()
    name <- str_extract(datafile, '^[a-zA-Z]*')
    print(paste0("Processing ", name, ", ", comparison_period, "..."))

    # Calculate which layers to include, per comparison_period
    file_start_date <- as.Date(paste0(str_extract(datafile, '(?<=_)[0-9]{6}'), '01'), '%Y%m%d')
    file_end_date <- as.Date(paste0(str_extract(datafile, '(?<=-)[0-9]{6}(?=.)'), '01'), '%Y%m%d')
    file_dates <- seq(file_start_date, file_end_date, by='month')

    period_start <- str_extract(comparison_period, '^[0-9]*(?=-)')
    period_start_date <- as.Date(paste0(period_start, '01'), '%Y%m%d')
    period_end <- str_extract(comparison_period, '(?<=-)[0-9]*')
    period_end_date <- as.Date(paste0(period_end, '01'), '%Y%m%d')
    period_dates <- seq(period_start_date, period_end_date, by='month')

    out_basename <- file.path(in_folder, file_path_sans_ext(datafile))
    out_basename <- gsub('(?<=_)[0-9]{6}', period_start, out_basename, perl=TRUE)
    out_basename <- gsub('(?<=-)[0-9]{6}', period_end, out_basename, perl=TRUE)

    chirps <- stack(file.path(in_folder, datafile), bands=which(file_dates %in% period_dates))

    calc_annual_total <- function(p, ...) {
        p[p == -9999] <- NA
        # Don't include partial years
        n_years <- floor(dim(p)[3] / 12)
        out <- foreach(n=1:n_years, .combine=abind) %do%{
            start_layer <- 1 + (n - 1) * 12
            end_layer <- n * 12
            apply(p[, , start_layer:end_layer], c(1, 2), FUN=sum, na.rm=TRUE)
        }
        array(out, dim=c(dim(p)[1], dim(p)[2], ceiling(dim(p)[3]/12)))
    }
    annual_total <- rasterEngine(p=chirps, fun=calc_annual_total,
        datatype='FLT4S', outbands=floor(nlayers(chirps) / 12),
        outfiles=1, processing_unit="chunk", 
        filename=paste0(out_basename, '_annualtotal'), .packages='abind')

    coef_var <- cv(annual_total)

    writeRaster(coef_var,
                filename=paste0(out_basename, '_interannualvariability_pct.tif'),
                overwrite=TRUE)
}
