###############################################################################
# Calculates mean precipitation for GRP countries.
###############################################################################

source('../0_settings.R')

library(rgdal)
library(stringr)
library(raster)
library(ggplot2)
library(tools)
library(spatial.tools)
library(doParallel)
library(Rcpp)

cl  <- makeCluster(2)
registerDoParallel(cl)

# in_folder <- file.path(prefix, "GRP", "CHIRPS-2.0")
# out_folder <- file.path(prefix, "GRP", "CHIRPS-2.0")
in_folder <- file.path(prefix, "Vital_Signs", "CHIRPS-2.0")
out_folder <- file.path(prefix, "Vital_Signs", "CHIRPS-2.0")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))

datafiles <- dir(in_folder, pattern='_mean_monthly.grd$')
datafile <- datafiles[1]

foreach (datafile=datafiles) %do% {
    timestamp()
    name <- str_extract(datafile, '^[a-zA-Z]*')
    print(paste0("Processing ", name, "..."))
    out_basename <- file.path(in_folder, file_path_sans_ext(datafile))
    
    mean_mthly <- brick(file.path(in_folder, datafile))

    wet_seasons <- function(wm, mm, ...) {
        sourceCpp('4_calc_seasons.cpp')
        identify_seasons(wm, mm, dim(wm))
    }

    wet_seasons <- rasterEngine(wm=peak_wet_months, mm=mean_mthly,
        fun=wet_seasons, datatype='INT2S', outbands=12, outfiles=1, 
        processing_unit="chunk", .packages=c('Rcpp'),
        filename=paste0(out_basename, '_wetseasons'))

    # # Filter out hyperarid regions
    # maxima[annual_total < 25] <- 0
    # # Filter out maxima where maxima is less than 10% of annual total
    # maxima[(mm / annual_total) < .10] <- 0
    # maxima

mms <- as.array(mean_mthly)

sourceCpp('4_calc_seasons.cpp')
plot_seasonal(mms[1, 1, ], as.numeric(identify_seasons(mms[1, 1, ], c(1, 1, 12))))
plot_seasonal(mms[100, 100, ], as.numeric(identify_seasons(mms[100, 100, ], c(1, 1, 12))))
plot_seasonal(mms[200, 200, ], as.numeric(identify_seasons(mms[200, 200, ], c(1, 1, 12))))

plot_seasonal(mms[1, 1, ], Ckmeans.1d.dp(mms[1, 1, ], c(2,4))$cluster)
plot_seasonal(mms[100, 100, ], Ckmeans.1d.dp(mms[100, 100, ], c(2,4))$cluster)
plot_seasonal(mms[200, 200, ], Ckmeans.1d.dp(mms[200, 200, ], c(2,4))$cluster)

wss <- as.array(wet_seasons)

}
timestamp()

# wm: wet season maxima (boolean for each month - is each indiv month a wet 
# season maxima?)
# mm: mean monthly precip
# ws: wet season indicators (numeric indicators numbering wet seasons)
plot_seasonal <- function(mm, ws) {
    d <- data.frame(month=factor(month.name, ordered=TRUE, levels=month.name),
                    precip=as.numeric(mm),
                    season=factor(ws, ordered=TRUE, levels=0:max(ws)))
    ggplot(d) + geom_bar(aes(month, precip, fill=season), stat='identity') +
        xlab('Month') +
        ylab('Mean precipitation (mm)') +
        guides(colour=FALSE)# + scale_fill_brewer(type='qual')
}

