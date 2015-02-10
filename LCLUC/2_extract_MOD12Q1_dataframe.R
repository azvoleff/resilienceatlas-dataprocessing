library(raster)
library(stringr)
library(reshape2)
library(ggplot2)
library(scales) # for percent format
library(foreach)
library(lubridate)

source('../0_settings.R')

base_dir <- 'H:/Data/MODIS'

in_folder <- file.path(prefix, "GRP", "MODIS", 'MCD12Q1')

tifs <- dir(in_folder, pattern='.tif$')

tile_key <- read.csv('GRP_MODIS_key.csv')

band_num <- 1

gg_lcluc_fill <- scale_fill_manual("Land cover",
                         breaks=c("0", "1", "2", "3", "4", "5", "254"),
                         labels=c('Water', # 0 
                                  'Forest', # 1
                                  'Grassland/shrubland', # 2
                                  'Cropland', # 3
                                  'Urban', # 4
                                  'Cropland/natural mosaic', # 5
                                  'Unclassified'), # 254
                          values=c('#4AB1E1', # water
                                   '#1A7B02', # forest
                                   '#2FF26D', # grassland/shrubland
                                   '#917B22', # cropland
                                   '#DD5415', # urban
                                   '#F5F90C', # Cropland/natural mosaic
                                   '#101010'), # unclassified
                          drop=FALSE)

gg_lcluc_colour <- scale_colour_manual("Land cover",
                         labels=c('Water', # 0 
                                  'Forest', # 1
                                  'Grassland/shrubland', # 2
                                  'Cropland', # 3
                                  'Urban', # 4
                                  'Cropland/natural mosaic', # 5
                                  'Unclassified'), # 254
                          values=c('#4AB1E1', # water
                                   '#1A7B02', # forest
                                   '#2FF26D', # grassland/shrubland
                                   '#917B22', # cropland
                                   '#DD5415', # urban
                                   '#F5F90C', # Cropland/natural mosaic
                                   '#101010'), # unclassified
                          drop=FALSE)

# Below is based on IGBP product layer
reclass_mat <- matrix(c(0, 0, # Water
                        1, 1, # Evergreen needleleaf forest
                        2, 1, # Evergreen broadleaf forest
                        3, 1, # Deciduous needleleaf forest
                        4, 1, # Deciduous broadleaf forest
                        5, 1, # Mixed forests
                        6, 2, # Closed shrublands
                        7, 2, # Open shrublands
                        8, 2, # Woody savannas
                        9, 2, # Savannas
                        10, 2, # Grasslands
                        11, 0, # Permanent wetlands
                        12, 3, # Croplands
                        13, 4, # Urban and built-up
                        14, 5, # Cropland/natural vegetation mosaic
                        15, 0, # Snow and ice
                        16, 2, # Barren or sparsely vegetated
                        254, 254), # Unclassified
                        ncol=2, byrow=TRUE) 

aois <- data.frame(sitename="Niger",
                   path=file.path(prefix, 'GRP', 'Boundaries', 'National'),
                   layer="NER_adm0", stringsAsFactors=FALSE)

tile_key <- tile_key[tile_key$sitename == "Niger", ]

lcluc <- foreach (sitename=unique(tile_key$sitename),
                  .combine=rbind, .inorder=FALSE,
                  .packages=c("raster", "reshape2", "stringr",
                              'lubridate')) %do% {
    timestamp()
    sitecode <- gsub(' ', '', sitename)
    message('Processing ', sitecode, '...')

    these_tifs <- tifs[grepl(paste0('_', sitecode, '_'), tifs)]
    dates <- as.Date(str_extract(these_tifs, '_[0-9]{7}'), '_%Y%j')

    these_tifs <- these_tifs[order(dates)]

    these_tifs <- file.path(in_folder, these_tifs)

    img_stack <- stack(lapply(these_tifs, function(x) raster(x, nlayer=band_num)))

    #img_stack <- reclassify(img_stack, reclass_mat)

    freqs <- freq(img_stack, merge=TRUE)
    names(freqs)[names(freqs) == 'value'] <- 'class'
    this_lcluc <- melt(freqs, id.vars='class', variable.name="year", 
                       value.name="n_pixels")
    this_lcluc$year <- year(as.Date(str_extract(this_lcluc$year, "[0-9]{7}$"), "%Y%j"))
    this_lcluc$n_pixels[is.na(this_lcluc$n_pixels)] <- 0

    this_lcluc$sitecode <- sitecode
    this_lcluc$frac_pixels <- this_lcluc$n_pixels / ncell(img_stack[[1]])

    return(this_lcluc)
}
save(lcluc, file='MODIS_LCLUC_IBGP_recode.RData')
