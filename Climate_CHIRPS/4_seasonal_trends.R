###############################################################################
# Calculates mean precipitation for GRP countries.
###############################################################################

source('../0_settings.R')

library(rgdal)
library(stringr)
library(raster)
library(ggplot2)
library(gdalUtils)
library(tools)
library(spatial.tools)
library(Rcpp)

#dataset <- 'pentad'
dataset <- 'monthly'

# Over what period should the calculations be made?
mean_monthly_period <- '198501-201606'

in_folder <- file.path(prefix, "CHIRPS-2.0", paste0('global-', dataset))
out_folder <- file.path(prefix, "Resilience_Atlas", "CHIRPS-2.0")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))

chpclim_tifs <- dir(file.path(prefix, 'CHPclim'), pattern='^CHPclim\\.[01][0-9]\\.tif$')

chp <- stack(file.path(prefix, 'CHPclim', chpclim_tifs))

out_basename <- 'CHPclim_seasons'

###############################################################################
###############################################################################
### Debugging only
TZA <- readOGR(file.path(prefix, 'Global/GADM/'), 'TZA_adm0')
chp <- crop(chp , TZA)
###############################################################################
###############################################################################

timestamp()


# Try a new approach - minimize the mean variance between clusters, with a 
# minimum season length of three months


wet_seasons <- function(mm, ...) {
    sourceCpp('4_calc_seasons.cpp')
    #identify_seasons(wm, mm, dim(wm))
    identify_seasons(mm, dim(mm))
}

wet_seasons <- rasterEngine(mm=chp, fun=wet_seasons, datatype='INT2S', 
                            outbands=12, outfiles=1, processing_unit="chunk", 
                            .packages=c('Rcpp'),
                            filename=paste0(out_basename, '_wetseasons'))

# # Filter out hyperarid regions
# maxima[annual_total < 25] <- 0
# # Filter out maxima where maxima is less than 10% of annual total
# maxima[(mm / annual_total) < .10] <- 0
# maxima

chp <- as.array(chp)

sourceCpp('4_calc_seasons.cpp')
plot_seasonal(chp[1, 1, ], as.numeric(identify_seasons(chp[1, 1, ], c(1, 1, 12))))
plot_seasonal(chp[100, 100, ], as.numeric(identify_seasons(chp[100, 100, ], c(1, 1, 12))))
plot_seasonal(chp[200, 200, ], as.numeric(identify_seasons(chp[200, 200, ], c(1, 1, 12))))

plot_seasonal(chp[1, 1, ], Ckmeans.1d.dp(chp[1, 1, ], c(2,4))$cluster)
plot_seasonal(chp[100, 100, ], Ckmeans.1d.dp(chp[100, 100, ], c(2,4))$cluster)
plot_seasonal(chp[200, 200, ], Ckmeans.1d.dp(chp[200, 200, ], c(2,4))$cluster)

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

