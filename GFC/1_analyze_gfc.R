source('../0_settings.R')

library(gfcanalysis)
library(raster)
library(sp)
library(maptools)
library(rgdal)
library(tools)
library(stringr)
library(rgeos)
library(reshape2)
library(foreach)
library(doParallel)

###############################################################################
# Setup parameters
###############################################################################

ISO_2s <- c("ET")
ISO_2s <- c("ET", "DJ", "ER", "SO")

buffer_aoi <- FALSE

overwrite <- TRUE
forest_thresholds <- c(75)
to_utm <- FALSE

# Specify output_folder and data_folder relative to above prefixes
output_folder <- file.path(prefix, "GRP", "GFC")
data_folder <- file.path(prefix, "GRP", "GFC")
shp_folder <- file.path(prefix, "GRP", "Boundaries", "National")
stopifnot(file_test("-d", shp_folder))
stopifnot(file_test('-d', output_folder))
stopifnot(file_test('-d', data_folder))

iso_key <- read.csv("../ISO_Codes.csv")

# Setup possible locations for temp files
temps <- c('H:/Temp', # Buffalo drive
           'O:/Temp', # Blue drive (HP or thinkpad)
           '/localdisk/home/azvoleff/Temp', # vertica1
           'D:/Temp') # CI-TEAM

###############################################################################
# Script begins below
###############################################################################

if (to_utm) {
    utm_string <- '_utm'
} else {
    utm_string <- '_wgs84'
}

# Ensure all output_folders exist, to avoid race conditions
for (forest_threshold in forest_thresholds) {
    this_output_folder <- file.path(output_folder,
                               paste0(gsub('_', '', utm_string), '_', 
                                      forest_threshold, 'pct'))
    if (!file_test('-d', this_output_folder)) {
        print(paste(this_output_folder, 'does not exist - creating it'))
        dir.create(this_output_folder)
    } else if (overwrite) {
        warning(paste(this_output_folder, "already exists - existing files will be overwritten"))
    } else {
        stop(paste(this_output_folder, "already exists"))
    }
}

cl <- makeCluster(n_cpus)
registerDoParallel(cl)

aois <- foreach (ISO_2=ISO_2s, .combine=c) %do% {
    ISO_3 <- as.character(iso_key$ISO_3[match(ISO_2, iso_key$ISO_2)])

    aoi <- readOGR(shp_folder, paste0(ISO_3, '_adm0'))
    stopifnot(length(aoi) == 1)
    if (buffer_aoi) {
        orig_proj4string <- proj4string(aoi)
        aoi <- spTransform(aoi, CRS(utm_zone(aoi, proj4string=TRUE)))
        aoi <- gBuffer(aoi, width=100000)
        aoi <- spTransform(aoi, CRS(orig_proj4string))
    }
    aoi$label <- "National boundary"
    aoi <- as(aoi, 'SpatialPolygonsDataFrame')
    aoi <- aoi[, names(aoi) == 'label']
    aoi <- list(aoi)
    names(aoi) <- ISO_2
    return(aoi)
}

message('Starting gfcanalysis processing.')
foreach (forest_threshold=forest_thresholds) %:%
    foreach (aoi=aois, ISO_2=names(aois), .inorder=FALSE,
             .packages=c('gfcanalysis', 'raster', 'sp', 'maptools',
                         'rgdal', 'tools', 'rgeos', 'stringr')) %dopar% {
    raster_tmpdir <- file.path(temp, paste0('raster_',
                               paste(sample(c(letters, 0:9), 15), 
                                     collapse='')))
    dir.create(raster_tmpdir)
    rasterOptions(tmpdir=raster_tmpdir)

    this_output_folder <- file.path(output_folder,
                               paste0(gsub('_', '', utm_string), '_', 
                                      forest_threshold, 'pct'))

    sitename <- iso_key$COUNTRY[match(ISO_2, iso_key$ISO_2)]
    aoi_file <- file.path(this_output_folder,
                          paste0(ISO_2, '_aois', utm_string, '.RData'))
    save(aoi, file=aoi_file)

    sink_file <- file.path(this_output_folder,
                           paste0(ISO_2, '_sinkfile', utm_string, '.txt'))
    sink(sink_file, split=TRUE)

    timestamp()
    print(paste("Processing", sitename))

    tiles <- calc_gfc_tiles(aoi)
    print("Downloading data...")
    download_tiles(tiles, data_folder, first_and_last=FALSE)

    gfc_data_file <- file.path(this_output_folder,
                               paste0(ISO_2, '_gfcextract', utm_string, 
                                      '.tif'))
    if (overwrite || !file.exists(gfc_data_file)) {
        timestamp()
        print("Extracting GFC data...")
        gfc_data <- extract_gfc(aoi, data_folder, to_UTM=to_utm, 
                                filename=gfc_data_file, overwrite=TRUE)
    } else {
        gfc_data <- brick(gfc_data_file)
    }

    gfc_thresholded_file <- file.path(this_output_folder,
                                      paste0(ISO_2, '_gfcextract', 
                                             utm_string, '_threshold.tif'))
    if (overwrite || !file.exists(gfc_thresholded_file)) {
        timestamp()
        print("Thresholding GFC data...")
        gfc_thresholded <- threshold_gfc(gfc_data, 
                                         forest_threshold=forest_threshold, 
                                         filename=gfc_thresholded_file,
                                         overwrite=TRUE)
    } else {
        gfc_thresholded <- brick(gfc_thresholded_file)
    }

    gfc_stats_file <- file.path(this_output_folder,
                                paste0(ISO_2, '_gfcextract', utm_string, 
                                       '_stats_loss.csv'))
    if (overwrite || !file.exists(gfc_stats_file)) {
        timestamp()
        print("Generating annual GFC stats...")
        chg_stats <- gfc_stats(aoi, gfc_thresholded)
        write.csv(chg_stats$loss_table, file=gfc_stats_file, row.names=FALSE)
        gfc_gainstats_file <- file.path(this_output_folder,
                                        paste0(ISO_2, '_gfcextract', 
                                               utm_string, '_stats_gain.csv'))
        write.csv(chg_stats$gain_table, file=gfc_gainstats_file, 
                  row.names=FALSE)
    }

    gfc_annual_stack_file <- file.path(this_output_folder, paste0(ISO_2, 
    '_gfcextract', utm_string, '_annual.tif'))
    if (overwrite || !file.exists(gfc_annual_stack_file)) {
        timestamp()
        print("Generating annualized GFC stack...")
        gfc_annual_stack <- annual_stack(gfc_thresholded)
        writeRaster(gfc_annual_stack, filename=gfc_annual_stack_file, datatype='INT1U', 
                    overwrite=TRUE)
    } else {
        gfc_annual_stack <- brick(gfc_annual_stack_file)
    }

    # animations_folder <- file.path(this_output_folder, 'animations')
    # gfc_animation_file <- file.path(animations_folder,
    #                                 paste0(ISO_2, '.html'))
    # if (overwrite || !file.exists(gfc_animation_file)) {
    #     timestamp()
    #     print("Plotting animation(s)...")
    #     animate_annual(aoi, gfc_annual_stack, animations_folder, ISO_2, 
    #     sitename, type='html')
    #     # Below won't work on CI-TEAM unless imagemagicK is installed
    #     #animate_annual(aoi, gfc_annual_stack, "animations", ISO_2, 
    #     #sitename, type='gif')
    # }
    sink()
    removeTmpFiles(h=0)
    unlink(raster_tmpdir)
}

stopCluster(cl)

message('Finished gfcanalysis processing.')
