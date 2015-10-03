# Load data from S3

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

s3_out_bucket <- 'ci-vsdata'
s3_out_folder <- '/CHIRPS-2.0/results'

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

files <- arrange(files, scenario, variable, model, period, year)

# group_by(files, variable, model, scenario) %>%
#     summarise(n=length(year), first=min(year), last=max(year))


# give differences as relative to the 1980-2000 climatology.
files <- filter(files, year >= 1980, year <= 1999,
                variable == 'pr', scenario == 'historical')

min_year <- 1980
max_year <- 1981
start_day <- 1
stopifnot(start_day >= 1 & start_day <=365)
# For full year, set end_day to 365. Leap years are handled automatically.
end_day <- 365
stopifnot(end_day >= 1 & end_day <=365)
scenarios <- 'historical'
variables <- 'pr'

# Loop over models
foreach(this_variable=variables) %:% foreach(this_scenario=scenarios) %do% {
    file_basename <- paste0(this_variable, '_', this_scenario, '_', min_year, '-', 
                            max_year, '_', start_day, '-', end_day)
    these_files <- filter(files, year >= min_year, year <= max_year,
                          variable == this_variable, scenario == this_scenario,
                          model %in% c('CNRM-CM5', 'ACCESS1-0'))

    mean_totals <- foreach(this_model=unique(these_files$model),
                           .packages=c('abind', 'iterators'),
                           .combine=abind) %do% {
        print(paste0('Processing ', this_model))

        # Loop over files within this model
        totals <- foreach(s3file=iter(filter(these_files, model == this_model), by='row'), 
                            .packages=c('rhdf5'), .combine=abind) %dopar% {
            print(s3file)
            temp_file <- tempfile(fileext='.hdf')
            download.file(s3file$url, temp_file)

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

            # Calculate annual total and return as a 1 layer array
            return(array(d, dim=c(dim(d), 1)))
        }

        # Write array to S3
        out <- brick(nrows=720, ncols=1440, nl=dim(totals)[3],
                     xmn=-180, xmx=180, ymn=-90, ymx=90, crs='+init=epsg:4326')
        out <- setValues(out, totals)

        out_file <-  file.path(out_folder, paste0(file_basename, "_", this_model, ".tif"))
        writeRaster(out, out_file, overwrite=TRUE)
        system(paste('s3put -b', s3_out_bucket, '-p', out_folder, '-k', s3_out_folder, out_file))
        
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

    system(paste('s3put -b', s3_out_bucket, '-p', out_folder, '-k', s3_out_folder, out_file))
}
