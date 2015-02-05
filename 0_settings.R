PLOT_WIDTH <- 6.5
PLOT_HEIGHT <- 6.5
PLOT_DPI <- 300

prefixes <- c('C:/Data', # HP current work
              'S:/Data', # Shared blue drive
              'D:/azvoleff/Data', # CI-TEAM
              'H:/Data', # Buffalo drive
              'O:/Data', # Blue drive
              '/localdisk/home/azvoleff/Data') # vertica1
prefix <- prefixes[match(TRUE, unlist(lapply(prefixes, function(x) file_test('-d', x))))]

geogs <- read.csv(file.path(prefix, 'GRP', 'GRP_Countries.csv'))

temps <- c('C:/Temp', # Local
           'S:/Temp', # Shared Blue drive
           'H:/Temp', # Buffalo drive
           'O:/Temp', # Blue drive (HP or thinkpad)
           '/localdisk/home/azvoleff/Temp', # vertica1
           'D:/Temp') # CI-TEAM
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
    n_cpus <- 3
}
