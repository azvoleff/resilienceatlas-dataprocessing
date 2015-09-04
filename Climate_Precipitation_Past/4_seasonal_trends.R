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

    calc_peak_wet_months <- function(mm, ...) {
        # when used to scale mean_mthly
        annual_total <- apply(mm, c(1, 2), sum)
        # Converted to a vector so it will recycle properly when used to scale 
        # mean_mthly
        annual_total <- as.vector(annual_total)
        find_maxima <- function(p) {
            # Wrap series so that local maxima can be found at beginning or end 
            # of series
            p_wrap <- c(p, p, p)
            # Find indices of maxima
            indices <- which(diff(sign(diff(p_wrap))) == -2) + 1
            # Remove maxima that are repeated due to wrapping of series
            indices <- indices - length(p) # to allow local maxima in first position
            indices <- indices[(indices >= 1) & (indices <= length(p))]
            out <- rep(0, length(p))
            out[indices] <- 1
            out
        }
        maxima <- apply(mm, c(1, 2), find_maxima)
        # Reformat after apply so proper shape is retained
        maxima <- aperm(maxima, c(2, 3, 1))

        # Filter out hyperarid regions
        maxima[annual_total < 25] <- 0
        # Filter out maxima where maxima is less than 10% of annual total
        maxima[(mm / annual_total) < .10] <- 0
        maxima
    }

    peak_wet_months <- rasterEngine(mm=mean_mthly, fun=calc_peak_wet_months,
        datatype='INT2S', outbands=12, outfiles=1, processing_unit="chunk", 
        filename=paste0(out_basename, '_peakwetmonths'))
    plot(peak_wet_months)

    # Code the period of each wet or dry season by grouping months around a wet 
    # season peak so long as there was precip in that month that was equal to 
    # at least 30% of that wet season peak

    ### TESTING
    wm <- c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0)
    mm <- c(0, 10, 15, 8, 4, 2, 12, 18, 25, 20, 5, 3)
    # Year start peak:
    wm <- c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
    mm <- c(15, 8, 4, 2, 12, 18, 25, 20, 5, 3, 0, 10)
    # Year end peak:
    wm <- c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0)
    mm <- c(0, 10, 15, 8, 4, 2, 12, 18, 25, 20, 5, 3)
    ### TESTING

    wet_seasons <- function(wm, mm, ...) {
        sourceCpp('calc_missings.cpp')
        # Function to code seasonal data
        ident_seasons <- function(wm, mm) {
            # Extend series so seasons can wrap around beg/end of year
            wm_wrap <- c(wm, wm, wm)
            mm_wrap <- c(mm, mm, mm)
            # In forward direction, only need to process max indices in the first 
            # two thirds of the wrapped series
            fwd_indices <- which(wm_wrap == 1)
            fwd_indices <- fwd_indices[fwd_indices <= (2*length(wm))]
            # In backwards direction, only need to process max indices in the two 
            # second two thirds of the wrapped series
            bwd_indices <- which(wm_wrap == 1)
            bwd_indices <- bwd_indices[bwd_indices > length(wm)]
            foreach (max_i=fwd_indices) %do% {
                lag_n <- 1
                while (lag_n <= 6) {
                    # if ((mm_wrap[max_i + lag_n] >= .3*mm_wrap[max_i]) |
                    #     (mm_wrap[max_i + lag_n] >= 2*mm_wrap[max_i + lag_n + 1])) {
                    if (mm_wrap[max_i + lag_n] >= .3*mm_wrap[max_i]) {
                        wm_wrap[max_i + lag_n] <- 1
                        lag_n <- lag_n + 1
                    } else {
                        break
                    }
                }
            }
            foreach (max_i=bwd_indices) %do% {
                lag_n <- 1
                while (lag_n <= 6) {
                    # if ((mm_wrap[max_i - lag_n] >= .3*mm_wrap[max_i]) |
                    #     (mm_wrap[max_i - lag_n] >= 2*mm_wrap[max_i - lag_n - 1])) {
                    if (mm_wrap[max_i - lag_n] >= .3*mm_wrap[max_i]) {
                        wm_wrap[max_i - lag_n] <- 1
                        lag_n <- lag_n + 1
                    } else {
                        break
                    }
                }
            }
            wm_wrap[(length(wm) + 1):(2 * length(wm))]
        }

        seasons <- foreach(x=1:dim(wm)[1]) %:% foreach(y=1:dim(wm)[2]) %do% {
            ident_seasons(as.vector(wm[x, y, ]), as.vector(mm[x, y, ]))
        }
        as.array(seasons, dim=dim(wm))
    }

    wet_seasons <- rasterEngine(wm=peak_wet_months, mm=mean_mthly,
        fun=wet_seasons, datatype='INT2S', outbands=12, outfiles=1, 
        processing_unit="chunk", .packages=c('foreach', 'Rcpp', 'inline'),
        filename=paste0(out_basename, '_wetseasons'))
    
    plot(sum(peak_wet_months))

}
timestamp()

# wm: wet season maxima (boolean for each month - is each indiv month a wet 
# season maxima?)
# mm: mean monthly precip
# ws: wet season indicators (numeric indicators numbering wet seasons)
plot_seasonal <- function(wm, mm=NA, ws=NA) {
    d <- data.frame(month=factor(month.name, ordered=TRUE, levels=month.name),
                    precip=as.numeric(mm),
                    maxima=as.logical(wm),
                    season=factor(ws))
    ggplot(d) + geom_bar(aes(month, precip, fill=season), stat='identity') +
        xlab('Month') +
        ylab('Mean precipitation (mm)') +
        guides(colour=FALSE) + scale_fill_brewer(type='qual')
}

