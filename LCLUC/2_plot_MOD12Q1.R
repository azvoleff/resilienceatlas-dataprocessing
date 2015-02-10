library(raster)
library(stringr)
library(reshape2)
library(plyr)
library(ggplot2)
library(scales) # for percent format
library(foreach)

source('../0_settings.R')

base_dir <- 'H:/Data/MODIS'

in_folder <- file.path(prefix, "GRP", "MODIS", 'MCD12Q1')

tifs <- dir(in_folder, pattern='.tif$')

tile_key <- read.csv('GRP_MODIS_key.csv')

band_num <- 1

width <- 10
height <- 7.5
dpi <- 300

gg_general <- theme_grey(base_size=18)
gg_transparent <- theme(legend.position="bottom", 
                        axis.text=element_text(colour='white'), 
                        axis.title.x=element_text(colour='white'), 
                        axis.title.y=element_text(colour='white'), 
                        plot.background=element_rect(fill='transparent', colour=NA),
                        legend.text=element_text(colour='white'), 
                        legend.title=element_text(colour='white'), 
                        legend.background=element_rect(fill='transparent', colour=NA))

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
                  .packages=c("raster", "reshape2", "stringr")) %do% {
    timestamp()
    sitecode <- gsub(' ', '', sitename)
    message('Processing ', sitecode, '...')

    these_tifs <- tifs[grepl(paste0('_', sitecode, '_'), tifs)]
    dates <- as.Date(str_extract(these_tifs, '_[0-9]{7}'), '_%Y%j')

    these_tifs <- these_tifs[order(dates)]

    these_tifs <- file.path(in_folder, these_tifs)

    img_stack <- stack(lapply(these_tifs, function(x) raster(x, nlayer=band_num)))

    img_stack <- reclassify(img_stack, reclass_mat)

    freqs <- freq(img_stack, merge=TRUE)
    names(freqs)[names(freqs) == 'value'] <- 'class'
    this_lcluc <- melt(freqs, id.vars='class')
    names(this_lcluc)[names(this_lcluc) == 'value'] <- 'n_pixels'

    this_lcluc$year_num <- as.numeric(gsub('layer.', '', this_lcluc$variable))
    this_lcluc$year <- dates[this_lcluc$year_num]

    # Drop unneeded columns
    this_lcluc <- this_lcluc[!(names(this_lcluc) %in% c('variable', 'year_num'))]

    this_lcluc$sitecode <- sitecode
    this_lcluc$frac_pixels <- this_lcluc$n_pixels / ncell(img_stack[[1]])

    return(this_lcluc)
}

lcluc$class <- factor(lcluc$class, levels=c(0, 1, 2, 3, 4, 5, 254), 
                      labels=c('Water',
                               'Forest',
                               'Grassland/shrubland', 
                               'Cropland',
                               'Urban',
                               'Cropland/natural mosaic', 
                               'Unclassified'))

save(lcluc, file='MODIS_LCLUC_IBGP_recode.RData')

p <- ggplot(lcluc[!(lcluc$class %in% c('Water', 'Unclassified')), ]) + gg_general + 
    geom_line(aes(year, frac_pixels, colour=class), size=1.5) +
    facet_wrap(~sitecode) +
    scale_y_continuous(labels=percent_format()) +
    scale_x_date(breaks=date_breaks("3 years"), labels=date_format("%Y")) +
    ylab('Percent of site') + xlab('Year') +
    theme(panel.background=element_rect(fill='darkgrey', colour='grey')) +
    guides(colour=guide_legend(ncol=2)) +
    gg_lcluc_colour + gg_transparent
ggsave(paste0('MCD12Q1_all_sites_recode_percent.png'), width=width, 
       height=height, dpi=dpi, plot=p, bg='transparent')

###############################################################################
# Calculate relative percent ag versus forest cover
ag_vs_forest <- ddply(lcluc, .(sitecode, year), summarize,
                      total_pixels = 
                      ag_vs_forest=(sum(n_pixels[class %in% c('Cropland', 'Cropland/natural mosaic')]) / n_pixels[class =='Forest']),
                      SiteNamePretty=SiteNamePretty[1],
                      Continent=continent[1])
ag_vs_forest <- ag_vs_forest[!(ag_vs_forest$sitecode == 'NNN'), ]
ag_vs_forest <- ag_vs_forest[!(ag_vs_forest$sitecode == 'CSN'), ]
ag_vs_forest$year <- as.Date(paste0(ag_vs_forest$year, '/1/1'))

#                      urban_vs_forest=(n_pixels[class == 'Urban'] / 
#                      n_pixels[class =='Forest']),

ggplot(ag_vs_forest, aes(year, ag_vs_forest, colour=Continent)) + geom_line() + 
    facet_wrap(~SiteNamePretty, scale='free') + geom_smooth(method='lm') +
    xlab('Year') + scale_x_date(labels=date_format("'%y"), breaks='4 years') +
    ylab('Agriculture to forest ratio') + 
    theme_grey(base_size=18) + transparent_opts
ggsave(paste0('ag_to_forest.png'), width=width, height=height, dpi=dpi, 
       bg='transparent')

