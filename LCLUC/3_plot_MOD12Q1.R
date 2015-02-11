library(ggplot2)
library(scales) # for percent format
library(dplyr)

load('MODIS_LCLUC_IBGP_recode.RData')

width <- 10
height <- 7.5
dpi <- 300

gg_general <- theme_bw(base_size=18)
gg_transparent_opts <- theme(legend.position="bottom", 
                             axis.text=element_text(colour='white'), 
                             axis.title.x=element_text(colour='white'), 
                             axis.title.y=element_text(colour='white'), 
                             plot.background=element_rect(fill='transparent', colour=NA),
                             legend.text=element_text(colour='white'), 
                             legend.title=element_text(colour='white'), 
                             legend.background=element_rect(fill='transparent', colour=NA))

# lcluc$class <- factor(lcluc$class, levels=c(0, 1, 2, 3, 4, 5, 254), 
#                       labels=c('Water',
#                                'Forest',
#                                'Grassland/shrubland', 
#                                'Cropland',
#                                'Urban',
#                                'Cropland/natural mosaic', 
#                                'Unclassified'))


filter(lcluc, class == 16)

ggplot(filter(lcluc, class == 16)) + gg_general + 
    geom_line(aes(year, frac_pixels), size=1.5) +
    facet_wrap(~sitecode) +
    scale_y_continuous(labels=percent_format()) +
    scale_x_date(breaks=date_breaks("3 years"), labels=date_format("%Y")) +
    ylab('Percent of site') + xlab('Year') +
    theme(panel.background=element_rect(fill='darkgrey', colour='grey')) +
    guides(colour=guide_legend(ncol=2))

ggsave(paste0('MCD12Q1_all_sites_recode_percent.png'), width=width, 
       height=height, dpi=dpi, plot=p, bg='transparent')

p <- ggplot(lcluc[!(lcluc$class %in% c('Water', 'Unclassified')), ]) + gg_general + 
    geom_line(aes(year, frac_pixels, colour=class), size=1.5) +
    facet_wrap(~sitecode) +
    scale_y_continuous(labels=percent_format()) +
    scale_x_date(breaks=date_breaks("3 years"), labels=date_format("%Y")) +
    ylab('Percent of site') + xlab('Year') +
    theme(panel.background=element_rect(fill='darkgrey', colour='grey')) +
    guides(colour=guide_legend(ncol=2)) +
    gg_lcluc_colour
ggsave(paste0('MCD12Q1_all_sites_recode_percent.png'), width=width, 
       height=height, dpi=dpi, plot=p, bg='transparent')

###############################################################################
# Calculate relative percent ag versus forest cover
ag_vs_forest <- ddply(lcluc, .(sitecode, year), summarize,
                      ag_vs_forest=(sum(n_pixels[class %in% c('Cropland', 'Cropland/natural mosaic')]) / n_pixels[class =='Forest']))
ag_vs_forest$year <- as.Date(paste0(ag_vs_forest$year, '/1/1'))

#                      urban_vs_forest=(n_pixels[class == 'Urban'] / 
#                      n_pixels[class =='Forest']),

ggplot(ag_vs_forest, aes(year, ag_vs_forest)) + geom_line() + 
    facet_wrap(~sitecode, scale='free') + geom_smooth(method='lm') +
    xlab('Year') + scale_x_date(labels=date_format("'%y"), breaks='4 years') +
    ylab('Agriculture to forest ratio') + 
    theme_grey(base_size=18)
ggsave(paste0('ag_to_forest.png'), width=width, height=height, dpi=dpi)
