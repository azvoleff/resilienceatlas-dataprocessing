PLOT_WIDTH <- 6.5
PLOT_HEIGHT <- 6.5
PLOT_DPI <- 300

prefixes <- c('C:/Data', # HP current work
              'H:/Data', # Buffalo drive
              '~/Data', # vertica1
              'O:/Data') # Blue drive
prefix <- prefixes[match(TRUE, unlist(lapply(prefixes, function(x) file_test('-d', x))))]

temps <- c('C:/Temp', # Local
           'H:/Temp', # Buffalo drive
           'O:/Temp', # Blue drive (HP or thinkpad)
           '~/temp', # vertica1
           '~/Temp', # CI-TEAM
           '/tmp')
temp <- temps[match(TRUE, unlist(lapply(temps, function(x) file_test('-d', x))))]

library(raster)
rasterOptions(tmpdir=temp)

# Specify how many processors to use for parallel processing. On CI-TEAM, this 
# should be set to 6. On your laptop, set it somewhere between 2 and 4.
if (Sys.info()[4] == 'CI-TEAM') {
    n_cpus <- 8
} else if (Sys.info()[4] == 'vertica1.team.sdsc.edu') {
    n_cpus <- 16
} else {
    n_cpus <- 2
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

