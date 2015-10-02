# Load data from S3

library(RCurl)
library(jsonlite)
library(stringr)
library(dplyr)
library(rhdf5)
library(foreach)

# List available datasets
s3files <- fromJSON("https://nasanex.s3.amazonaws.com/NEX-GDDP/nex-gddp-s3-files.json")
urls <- names(s3files)
files <- data.frame(matrix(unlist(str_split(basename(urls), '_')), 
                           nrow=length(urls), byrow=TRUE))
names(files) <- c('variable', 'period', 'method', 'scenario', 'something', 
                  'model', 'year')
files$year <- as.numeric(gsub('.nc', '', files$year))
files$url <- as.character(urls)

# group_by(files, variable, model, scenario) %>%
#     summarise(n=length(year), first=min(year), last=max(year))


# give differences as relative to the 1980-2000 climatology.
files <- filter(files, year >= 1980, year <= 1999,
                variable == 'pr', scenario == 'historical')

### TESTING ##################
s3file <- precip_clim_files[1, ]
### TESTING ##################

# Loop over models
mean_ann_tots <- foreach(model=unique(files$model), .packages=c('abind'), 
                         .combind=abind) %dopar% {
    # Loop over files within this model
    ann_tots <- foreach(s3file=iter(filter(files, model == model), by='row'), 
                        .packages=c('rhdf5'), .combine=abind) %do% {
        outfile <- tempfile(fileext='.hdf')
        download.file(s3file$url, outfile)

        d <- h5read(outfile, paste0('/', s3file$variable))

        # Calculate annual total
        a <- apply(d, c(1,2), sum)

        return(array(a, dim=c(dim(a)[1], dim(a)[2], 1)))
    }
    # Calculate mean for this model
    return(apply(d, c(1, 2), mean))
}
