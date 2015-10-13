library(raster)
library(foreach)
library(stringr)

source('../0_settings.R')

in_folder <- file.path(prefix, "GRP", "CMIP5")

# Figure out what historical baselines we have
base_files <- list.files(in_folder, pattern="historical")
seasons <- unique(gsub('_', '',
                       str_extract(base_files, '_[0-9]{1,3}-[0-9]{1,3}_')))

periods <- c('2040-2059', '2080-2099')
scenarios <- c('rcp45', 'rcp85')
foreach(base_file=base_files) %do% {
    variable <- str_extract(base_file, '^[a-z]*')
    season <- gsub('_', '', str_extract(base_file, '_[0-9]{1,3}-[0-9]{1,3}_'))
    historical <- brick(file.path(in_folder, base_file))
    hist_mean <- mean(historical)
    
    foreach(scenario=scenarios) %:% foreach(period=periods) %do% {
        out_base <- paste(variable, scenario, period, season, sep='_')
        fut_file <- file.path(in_folder, paste0(out_base, '_modelmeans.tif'))
        if (!file_test('-f', fut_file)) {
            warning(paste(fut_file, 'does not exist'))
            return()
        }
        future <- brick(fut_file)
        fut_mean <- mean(future)
        pct_diff <- ((fut_mean - hist_mean) / hist_mean) * 100
        writeRaster(pct_diff, filename=file.path(in_folder, paste0(out_base, '_pctdiff.tif')), overwrite=TRUE)
    }
}
