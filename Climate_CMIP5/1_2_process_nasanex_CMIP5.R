library(RCurl)
library(jsonlite)
library(stringr)
library(dplyr)
library(rhdf5)
library(abind)
library(iterators)
library(raster)
library(rgdal)
library(tools)
library(foreach)
library(doParallel)

s3_out <- 's3://ci-vsdata/CMIP5/seasonal_totals/'

source('../ec2/get_cluster_hosts.R')

# Note that master node is excluded from workers
cl <- makeCluster(rep(get_cluster_hosts(FALSE), each=4))
registerDoParallel(cl)

# List available datasets
nex_files <- fromJSON("https://nasanex.s3.amazonaws.com/NEX-GDDP/nex-gddp-s3-files.json")
urls <- names(nex_files)
files <- data.frame(matrix(unlist(str_split(basename(urls), '_')), 
                           nrow=length(urls), byrow=TRUE))
names(files) <- c('variable', 'period', 'method', 'scenario', 'something', 
                  'model', 'year')
files$year <- as.numeric(gsub('.nc', '', files$year))
files$url <- as.character(urls)
# Convert URLs to S3 paths
files$url <- gsub('http://nasanex.s3.amazonaws.com', 's3://nasanex', files$url)

files$agg_period <- NA
files$agg_period[files$year >= 1986 & files$year <= 2005] <- '1986-2005'
files$agg_period[files$year >= 2040 & files$year <= 2059] <- '2040-2059'
files$agg_period[files$year >= 2080 & files$year <= 2099] <- '2080-2099'

files <- files[!is.na(files$agg_period), ]

chosen_variables <- c('pr', 'tasmax', 'tasmin')

# Assign periods
base_files <- filter(files, scenario == 'historical',
                     variable %in% chosen_variables)

fut_scenarios <- c('rcp45', 'rcp85')

seasons <- read.csv('seasons.csv')
stopifnot(sum(seasons[1] < seasons[2]) == nrow(seasons))

fut_files <- filter(files, scenario %in% fut_scenarios,
                    variable %in% chosen_variables)

# Process both baseline and future files
in_files <- rbind(base_files, fut_files)

# Function to sum or take mean of layers of CMIP5 hdf5 files in block-by-block 
# fashion to reduce memory usage
aggregate_h5_layers <- function(filename, datasetname, first_layer, last_layer, 
                                dims, variable, blocksize=100) {
    stopifnot(last_layer >= first_layer)
    stopifnot(variable %in% c('pr', 'tasmax', 'tasmin'))
    n_layers <- (last_layer - first_layer + 1)
    if (n_layers > 20) {
        start_rows <- seq(1, dims[1], blocksize)
        end_rows <- c(start_rows[2:length(start_rows)] - 1, dims[1])
        # Sum up in spatial rather than temporal blocks to avoid floating point 
        # errors
        d_sum <- foreach(start_row=start_rows, end_row=end_rows,
                         .combine=rbind) %do% {
            d <- h5read(filename, datasetname,
                        index=list(c(start_row:end_row), NULL, 
                                   first_layer:last_layer))
            # Remove out of range values
            d[d > 400] <- NA
            apply(d, c(1, 2), sum, na.rm=TRUE)
        }
    } else {
        d <- h5read(filename, datasetname, index=list(NULL, NULL, c(first_layer:last_layer)))
        # Remove out of range values
        d[d > 400] <- NA
        d_sum  <- apply(d, c(1, 2), sum, na.rm=TRUE)
    }
    if (variable %in% c('tasmax', 'tasmin')) {
        # Take mean of temperature layers.
        #
        # Note that CMIP5 data is in Kelvin, so subtract 273.15 to get it in 
        # Celsius.
        out <- d_sum/n_layers - 273.15
    } else {
        # Sum the precipitation layers.
        #
        # Note that CMIP5 data is in kg/m^2/s. 
        # This is the same as mm/s, so multiply by 3600 * 24 to get mm/day. 
        # Note that since precip has been summed it is (prior to conversion 
        # below) in units of average kg/m^2/n_layers where n_layers is number 
        # of days. So no need to multiple by number of layers at this stage.
        out <- d_sum*3600*24
    }
    # NAs got recoded as zeros - recode them back to NA so the rasters will 
    # write correctly
    out[out == 0] <- NA
    H5close()
    return(out)
}

###############################################################################
### Testing
# in_file <- in_files[1, ]
# season <- seasons[1, ]
# Sys.setenv(AWS_CONFIG_FILE='C:/Users/azvoleff/.aws/config')
###############################################################################

aws_cp <- function(from, to) {
    system2('aws', args=c('s3', 'cp', from, to, '--region=us-east-1'), stdout=NULL)
}

# Loop over models
timestamp()
print('Processing daily files...')
foreach(in_file=iter(in_files, by='row'),
        .packages=c('rhdf5', 'foreach', 'raster', 'rgdal',
                    'iterators', 'tools')) %dopar% {
    temp_hdf <- tempfile(fileext='.hdf')
    aws_cp(in_file$url, temp_hdf)
    stopifnot(file_test('-f', temp_hdf))

    # Read coordinates and convert so they can be read in properly
    lon <- h5read(temp_hdf, '/lon')
    # Convert longitudes to range between -180 and 180
    lon[lon > 180] <- lon[lon > 180] - 360
    lat <- h5read(temp_hdf, '/lat')

    # TODO: Handle case of end day that is less than start day, meaning 
    # that the season spans two calendar years.
    
    h5_meta <- h5ls(temp_hdf)

    n_days <- as.numeric(h5_meta[match('time', h5_meta$name), ]$dim)

    # Account for leap years
    season_adj <- seasons
    if(n_days == 366) {
        with(season_adj, start_day[start_day > 59] <- start_day[start_day > 59] + 1)
        with(season_adj, end_day[end_day > 59] <- end_day[end_day > 59] + 1)
    }

    foreach(season=iter(season_adj, by='row')) %do% {
        if (in_file$variable == 'pr') {
            agg_func <- 'sum'
        } else if (in_file$variable %in% c('tasmin', 'tasmax')) {
            agg_func <- 'mean'
        } else {
            stop(paste('unrecognized variable', in_file$variable))
        }

        d <- aggregate_h5_layers(temp_hdf,
                                 paste0('/', in_file$variable), 
                                 season$start_day, season$end_day,
                                 dims=c(length(lon), length(lat)),
                                 variable=in_file$variable)

        # Reorder rows and columns so they are ordered according to lat/long 
        # with ul corner having highest latitude and lowest longitude
        d <- t(d)
        d <- d[order(lat, decreasing=TRUE), ]
        d <- d[ , order(lon)]

        # Write to S3
        out <- raster(d, xmn=-180, xmx=180, ymn=-90, ymx=90, 
                      crs='+init=epsg:4326')
        temp_tif <- tempfile(fileext='.tif')
        writeRaster(out, temp_tif, overwrite=TRUE)
        s3_file <- paste0(file_path_sans_ext(basename(in_file$url)), 
                         '_', season$name, '_', agg_func, '.tif')
        aws_cp(temp_tif, paste0(s3_out, s3_file))
        unlink(temp_tif)
    }
    unlink(temp_hdf)
}

print("Finished processing.")
timestamp()
