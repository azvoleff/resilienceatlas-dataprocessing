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

s3_in <- 's3://ci-vsdata/CMIP5/seasonal_totals/'
s3_out <- 's3://ci-vsdata/CMIP5/results/'

s3_files <- s3_ls(s3_in)

s3_files$scenario <- str_extract(s3_files$file, '(historical)|(rcp(45|85))')
s3_files$variable <- str_extract(s3_files$file, '^[a-z]*')
s3_files$season <- str_extract(s3_files$file, '(?<=_)[a-zA-Z]*(?=_((sum)|(mean)))')
s3_files$model <- str_extract(s3_files$file, '(?<=_)[-a-zA-Z0-9]*(?=_[0-9]{4})')
s3_files$year <- as.numeric(str_extract(s3_files$file, '(?<=_)[0-9]{4}(?=_)'))

s3_files$agg_period <- NA
s3_files$agg_period[s3_files$year >= 1986 & s3_files$year <= 2005] <- '1986-2005'
s3_files$agg_period[s3_files$year >= 2040 & s3_files$year <= 2059] <- '2040-2059'
s3_files$agg_period[s3_files$year >= 2080 & s3_files$year <= 2099] <- '2080-2099'

writeRasterS3 <- function(r, S3_loc) {
    require(raster)
    temp_file <- tempfile(fileext='.tif')
    writeRaster(r, filename=temp_file)
    system2('aws', args=c('s3', 'cp', temp_file, S3_loc))
    unlink(temp_file)
}

# Function to generate a unique temporary directory name separate from the R 
# session temp folder.
get_tempdir <- function() {
    rand_str <- function() {
        paste(sample(c(0:9, letters, LETTERS), 10, replace=TRUE), collapse='')
    }
    rand_dir <- paste0(tempdir(), '_', rand_str())
    while(!dir.create(rand_dir, showWarnings=FALSE)) {
        rand_dir <- paste0(tempdir(), '_', rand_str())
    }
    return(rand_dir)
}

###############################################################################
### Testing
s3_files <- filter(s3_files, season == 'annual')
# this_variable <- 'pr'
# this_season <- 'annual'
# this_model <- 'ACCESS1-0'
###############################################################################

print('Processing...')
timestamp()
foreach(this_variable=unique(s3_files$variable)) %:%
    foreach(this_season=unique(s3_files$season)) %do% {

    base_files <- filter(s3_files, variable == this_variable,
                         season == this_season, scenario == 'historical')
    stopifnot(length(unique(base_files$agg_period)) == 1)
    base_agg_period <- base_files$agg_period[1]

    # Calculate baseline mean (base_m)
    base_m <- foreach(this_model=unique(base_files$model), .combine=stack,
                      .packages=c('raster', 'dplyr')) %dopar% {
        temp_dir <- get_tempdir()
        these_files <- filter(base_files, model == this_model)$file
        foreach(this_file=these_files) %do% {
            system2('aws', args=c('s3', 'cp', paste0(s3_in, this_file), temp_dir))
        }
        model_data <- stack(file.path(temp_dir, these_files))
        mod_mean <- mean(model_data)
        unlink(temp_dir, recursive=TRUE)
        return(mod_mean)
    }
    s3_out_base_m <- paste0(s3_out, paste(this_variable, 'historical', 
        base_agg_period, this_season, 'modelmeans.tif', sep='_'))
    writeRasterS3(base_m, s3_out_base_m)

    # Calculate multimodel mean for baseline (base_mmm)
    s3_out_base_mmm <- paste0(s3_out, paste(this_variable, 'historical', 
        base_agg_period, this_season, 'multimodelmean.tif', sep='_'))
    temp_file <- tempfile(fileext='.tif')
    base_mmm <- calc(base_m, mean, filename=temp_file)
    system2('aws', args=c('s3', 'cp', temp_file, s3_out_base_mmm))

    # Calculate multimodel sd for baseline (base_mmsd)
    s3_out_base_mmsd <- paste0(s3_out, paste(this_variable, 'historical', 
        base_agg_period, this_season, 'multimodelsd.tif', sep='_'))
    temp_file <- tempfile(fileext='.tif')
    base_mmsd <- calc(base_m, sd, filename=temp_file)
    system2('aws', args=c('s3', 'cp', temp_file, s3_out_base_mmsd))

    # Calculate scenario model means
    scenarios <- unique(s3_files$scenario[s3_files$scenario != 'historical'])
    agg_periods <- unique(s3_files$agg_period[s3_files$scenario != 'historical'])
    foreach(this_scenario=scenarios) %:% 
        foreach(this_agg_period=agg_periods,
                .packages=c('raster', 'dplyr', 'foreach')) %do% {

        scen_files <- filter(s3_files,
                             variable == this_variable,
                             season == this_season,
                             scenario == this_scenario,
                             agg_period == this_agg_period)

        scen_m <- foreach(this_model=unique(scen_files$model), .combine=stack,
                          .packages=c('raster', 'dplyr')) %dopar% {
            temp_dir <- get_tempdir()
            these_files <- filter(scen_files, model == this_model)$file
            foreach(this_file=these_files) %do% {
                system2('aws', args=c('s3', 'cp', paste0(s3_in, this_file), temp_dir))
            }
            model_data <- stack(file.path(temp_dir, these_files))
            mod_mean <- mean(model_data)
            unlink(temp_dir, recursive=TRUE)
            return(mod_mean)
        }
        s3_out_scen_m <- paste0(s3_out, paste(this_variable, this_scenario, 
            base_agg_period, this_season, 'modelmeans.tif', sep='_'))
        writeRasterS3(scen_m, s3_out_scen_m)

        # Calc scenario multimodel mean
        temp_file <- tempfile(fileext='.tif')
        scen_mmm <- calc(scen_m, mean, filename=temp_file)
        s3_out_scen_mmm <- paste0(s3_out, paste(this_variable, this_scenario, 
                                            this_agg_period, this_season, 
                                            'multimodelmean.tif', sep='_'))
        system2('aws', args=c('s3', 'cp', temp_file, s3_out_scen_mmm))

        # Calc scenario multimodel sd
        temp_file <- tempfile(fileext='.tif')
        scen_mmsd <- calc(scen_m, mean, filename=temp_file)
        s3_out_scen_mmsd <- paste0(s3_out, paste(this_variable, this_scenario, 
                                            this_agg_period, this_season, 
                                            'multimodelsd.tif', sep='_'))
        system2('aws', args=c('s3', 'cp', temp_file, s3_out_scen_mmsd))

        # Calculate percent difference
        temp_file <- tempfile(fileext='.tif')
        pct_diff <- overlay(scen_mmm, base_mmm, fun=function(scen, base) {
                ((scen - base) / base) * 100
            }, filename=temp_file)
        s3_out_pctdiff <- paste0(s3_out, paste(this_variable, this_scenario,
            'pctdiff', base_agg_period, 'vs', this_agg_period, sep='_'), '.tif')
        system2('aws', args=c('s3', 'cp', temp_file, s3_out_pctdiff))
    }
}
print("Finished processing.")
timestamp()
