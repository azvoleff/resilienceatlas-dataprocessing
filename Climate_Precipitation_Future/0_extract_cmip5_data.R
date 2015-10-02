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

### TESTING ##################
files <- filter(files, year >= 1980, year <= 1981,
                variable == 'pr', scenario == 'historical',
                model %in% c('CNRM-CM5', 'ACCESS1-0'))
this_model <- files$model[1]
s3file <- files[1, ]
### TESTING ##################

# Loop over models
mean_ann_tots <- foreach(this_model=unique(files$model),
                         .packages=c('abind', 'iterators'), 
                         .combine=abind) %do% {
    print(this_model)
    # Loop over files within this model
    ann_tots <- foreach(s3file=iter(filter(files, model == this_model), by='row'), 
                        .packages=c('rhdf5'), .combine=abind) %dopar% {
        print(s3file)
        outfile <- tempfile(fileext='.hdf')
        download.file(s3file$url, outfile)

        d <- h5read(outfile, paste0('/', s3file$variable))
        d <- apply(d, c(1, 2), sum)

        # Calculate annual total and return as a 1 layer array
        return(array(d, dim=c(dim(d), 1)))
    }
    # Calculate mean for this model, and make it into a 1 layer array
    mod_mean <- apply(ann_tots, c(1, 2), mean)

    # Write model mean to disk
    
    return(array(mod_mean, dim=c(dim(mod_mean), 1)))
}

out <- brick(nrows=720, ncols=1440, nl=dim(mean_ann_tots)[3],
             xmn=-180, xmx=180, ymn=-90, ymx=90, crs='+init=epsg:4326')
out <- setValues(out, mean_ann_tots)
writeRaster(out, file.path(out_folder, "mean_ann_tots.tif"), overwrite=TRUE)

