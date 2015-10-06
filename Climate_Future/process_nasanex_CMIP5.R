library(RCurl)
library(jsonlite)
library(stringr)
library(dplyr)
library(rhdf5)
library(abind)
library(iterators)
library(raster)
library(foreach)
library(doParallel)

s3_out <- 's3://ci-vsdata/CMIP5/results/'

cl <- makeCluster(4)
registerDoParallel(cl)

out_folder <- '~/temp'

# List available datasets
s3files <- fromJSON("https://nasanex.s3.amazonaws.com/NEX-GDDP/nex-gddp-s3-files.json")
urls <- names(s3files)
files <- data.frame(matrix(unlist(str_split(basename(urls), '_')), 
                           nrow=length(urls), byrow=TRUE))
names(files) <- c('variable', 'period', 'method', 'scenario', 'something', 
                  'model', 'year')
files$year <- as.numeric(gsub('.nc', '', files$year))
files$url <- as.character(urls)
# Convert URLs to S3 paths
files$url <- gsub('http://nasanex.s3.amazonaws.com', 's3://nasanex', files$url)

files <- arrange(files, scenario, variable, model, period, year)

min_year <- 1980
max_year <- 1999
start_day <- 1
stopifnot(start_day >= 1 & start_day <=365)
# For full year, set end_day to 365. Leap years are handled automatically.
end_day <- 365
stopifnot(end_day >= 1 & end_day <=365)
scenarios <- 'historical'
#scenarios <- c('rcp45', 'rcp85')
variables <- c('pr', 'tasmax', 'tasmin')

############# TESTING ONLY
# this_variable <- 'pr'
# this_scenario <- scenarios[1]
# this_model <- files$model[1]
# s3file <- files[1,]
############# /TESTING ONLY

# Loop over models
foreach(this_variable=variables) %:% foreach(this_scenario=scenarios) %do% {
    timestamp()
    print(paste0('Processing ', this_variable, ', ', this_scenario))
    file_basename <- paste0(this_variable, '_', this_scenario, '_', min_year, '-', 
                            max_year, '_', start_day, '-', end_day)
    these_files <- filter(files, year >= min_year, year <= max_year,
                          variable == this_variable, scenario == this_scenario)

    mean_totals <- foreach(this_model=unique(these_files$model),
                           .combine=abind,
                           .packages=c('abind', 'iterators', 'rhdf5', 
                                       'foreach', 'dplyr')) %dopar% {
        print(paste0('Processing ', this_model))

        # Loop over files from this model
        model_files <- filter(these_files, model == this_model)
        totals <- foreach(s3file=iter(model_files, by='row'), .combine=abind) %do% {
            temp_file <- tempfile(fileext='.hdf')
            print(paste(temp_file, s3file$url))
            system2('aws', args=c('s3', 'cp', s3file$url, temp_file))
            stopifnot(file_test('f', temp_file))

            # Read coordinates and convert so they can be read in properly
            lon <- h5read(temp_file, '/lon')
            # Convert longitudes to range between -180 and 180
            lon[lon > 180] <- lon[lon > 180] - 360
            lat <- h5read(temp_file, '/lat')

            d <- h5read(temp_file, paste0('/', s3file$variable))
            # TODO: Handle case of end day that is less than start day, meaning 
            # that the season spans two calendar years.
            if (!(start_day == 1 & end_day == 365)) {
                if (end_day == 365) {
                    if (dim(d)[3] == 366) {
                        this_end_day <- 366
                    } else {
                        this_end_day <- 365
                    }
                } else {
                    this_end_day <- end_day
                }
                d <- d[, , start_day:this_end_day]
            }
            d <- apply(d, c(1, 2), sum)
            d <- aperm(d, c(2, 1))

            # Reorder rows and columns so they are ordered according to 
            # lat/long with ul corner having highest latitude and lowest 
            # longitude
            d <- d[order(lat, decreasing=TRUE), ]
            d <- d[ , order(lon)]

            unlink(temp_file)

            # Calculate annual total and return as a 1 layer array
            return(array(d, dim=c(dim(d), 1)))
        }

        # Write array to S3
        out <- brick(nrows=720, ncols=1440, nl=dim(totals)[3],
                     xmn=-180, xmx=180, ymn=-90, ymx=90, crs='+init=epsg:4326')
        out <- setValues(out, totals)

        out_file <-  file.path(out_folder, paste0(file_basename, "_", this_model, ".tif"))
        writeRaster(out, out_file, overwrite=TRUE)
        system2('aws', args=c('s3', 'cp', out_file, s3_out))
        
        # Calculate mean for this model, and make it into a 1 layer array
        mod_mean <- apply(totals, c(1, 2), mean)

        # Transpose so raster will appear correctly.
        mod_mean <- t(mod_mean)

        return(array(mod_mean, dim=c(dim(mod_mean), 1)))
    }

    out <- brick(nrows=720, ncols=1440, nl=dim(mean_totals)[3],
                 xmn=-180, xmx=180, ymn=-90, ymx=90, crs='+init=epsg:4326')
    out <- setValues(out, mean_totals)
    out_file <- file.path(out_folder, paste0(file_basename, '_modelmeans.tif'))
    writeRaster(out, out_file, overwrite=TRUE)

    system2('aws', args=c('s3', 'cp', out_file, s3_out))
}

print("Finished processing.")
timestamp()
