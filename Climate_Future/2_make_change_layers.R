library(raster)
library(dplyr)
library(foreach)
library(stringr)
library(abind)
library(doParallel)

source('../0_settings.R')

source('../ec2/s3_ls.R')

source('../ec2/get_cluster_hosts.R')
cl <- makeCluster(rep(get_cluster_hosts(), each=4))
registerDoParallel(cl)

in_folder <- file.path(prefix, "GRP", "CMIP5")

s3_in <- 's3://ci-vsdata/CMIP5/seasonal_totals/'
s3_out <- 's3://ci-vsdata/CMIP5/results/'

s3_files <- s3_ls(s3_in)

s3_files$scenario <- str_extract(s3_files$file, '(historical)|(rcp(45|85))')
s3_files$variable <- str_extract(s3_files$file, '^[a-z]*')
s3_files$period <- str_extract(s3_files$file, '[0-9]{4}-[0-9]{4}')
s3_files$season <- gsub('_', '', str_extract(s3_files$file, '_[0-9]{1,3}-[0-9]{1,3}_'))

## testing
# this_variable <- 'pr'
# this_season <- '1-365'
## testing

foreach(this_variable=unique(s3_files$variable)) %:%
    foreach(this_season=unique(s3_files$season)) %do% {

    base_files <- filter(s3_files, variable == this_variable,
                         season == this_season, scenario == 'historical')
    stopifnot(length(unique(base_files$period)) == 1)
    base_period <- base_files$period[1]

    # Calculate baseline mean (base_m)
    base_m <- foreach(base_file=base_files$file, .combine=stack,
                      .packages=c('raster')) %
                          dopar% {
        temp_file <- tempfile(fileext='.tif')
        system2('aws', args=c('s3', 'cp', paste0(s3_in, base_file), temp_file))
        model_data <- brick(temp_file)
        mod_mean <- mean(model_data)
        unlink(temp_file)
        return(mod_mean)
    }
    s3_out_base_m <- paste0(s3_out, paste(this_variable, 'historical', 
        base_period, this_season, 'modelmeans.tif', sep='_'))
    temp_file <- tempfile(fileext='.tif')
    writeRaster(base_m, filename=temp_file)
    system2('aws', args=c('s3', 'cp', temp_file, s3_out_base_m))

    # Calculate multimodel mean for baseline (base_mmm)
    s3_out_base_mmm <- paste0(s3_out, paste(this_variable, 'historical', 
        base_period, this_season, 'multimodelmean.tif', sep='_'))
    temp_file <- tempfile(fileext='.tif')
    base_mmm <- calc(base_m, mean, filename=temp_file)
    system2('aws', args=c('s3', 'cp', temp_file, s3_out_base_mmm))

    # Calculate scenario model means
    scenarios <- unique(s3_files$scenario[s3_files$scenario != 'historical'])
    periods <- unique(s3_files$period[s3_files$scenario != 'historical'])
    foreach(this_scenario=scenarios) %:% {
        foreach(this_period=periods, .packages=c('raster', 'dplyr')) %dopar% {
            scen_files <- filter(s3_files,
                                 variable == this_variable,
                                 season == this_season,
                                 scenario == this_scenario,
                                 period == this_period)
                scen_m <- foreach(scen_file=scen_files$file, .combine=stack) %do% {
                    temp_file <- tempfile(fileext='.tif')
                    system2('aws', args=c('s3', 'cp', paste0(s3_in, scen_file), temp_file))
                    model_data <- brick(temp_file)
                    mod_mean <- mean(model_data)
                    unlink(temp_file)
                    return(mod_mean)
                }
                temp_file <- tempfile(fileext='.tif')
                s3_out_scen_m <- paste0(s3_out, paste(this_variable, this_scenario, 
                    base_period, this_season, 'modelmeans.tif', sep='_'))
                writeRaster(baseline, filename=temp_file)
                system2('aws', args=c('s3', 'cp', temp_file, s3_out_scen_m))
        }

        # Calc scenario multimodel mean
        temp_file <- tempfile(fileext='.tif')
        scen_mmm <- calc(scen_m, mean, filename=temp_file)
        s3_out_scen_mmm <- paste0(s3_out, paste(this_variable, scenario, 
                                            this_period, this_season, 
                                            'multimodelmean.tif', sep='_'))
        system2('aws', args=c('s3', 'cp', temp_file, s3_out_scen_mmm))

        # Calculate percent difference
        temp_file <- tempfile(fileext='.tif')
        pct_diff <- overlay(scen_mmm, base_mmm, fun=function(scen, base) {
                ((scen - base) / base) * 100
            }, filename=temp_file)
        s3_out_pctdiff <- paste0(s3_out, paste(this_variable, this_scenario,
            'pctdiff', base_period, 'vs', this_period, sep='_'), '.tif')
        system2('aws', args=c('s3', 'cp', temp_file, s3_out_pctdiff))

    }
}
