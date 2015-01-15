source('../0_settings.R')

library(ggplot2)
library(scales)
library(dplyr)

###############################################################################
# Setup parameters
###############################################################################

forest_thresholds <- c(25)

data_folder <- file.path(prefix, "GRP", "GFC", "GFW")
plot_folder <- file.path(prefix, "GRP", "GFC", "GFW")
stopifnot(file_test('-d', data_folder))

ISO_2 <- "ET"

gfc_data <- read.csv(paste0("H:/Data/GRP/GFC/GFW/", ISO_2, "_GFC.csv"))

gfc_data$min_percent_canopy_density <- factor(gfc_data$min_percent_canopy_density)
gfc_data$year <- as.Date(paste0('1/1/', gfc_data$year), '%m/%d/%Y')

# Plot by region
ggplot(filter(gfc_data, min_percent_canopy_density == 20)) +
    geom_line(aes(year, loss_percent, colour=region))

# Calculate national-level loss
gfc_data_natl <- group_by(gfc_data, min_percent_canopy_density, year) %>%
    summarize(extent_ha=sum(extent_ha),
              loss_ha=sum(loss_ha)) %>%
    mutate(loss_percent=c(NA, loss_percent=(loss_ha[2:length(loss_ha)]/extent_ha[1])*100))

ggplot(gfc_data_natl) + geom_line(aes(year, loss_percent, colour=min_percent_canopy_density))

ggplot(filter(gfc_data_natl, min_percent_canopy_density == 20)) +
    geom_line(aes(year, loss_percent / 100), colour="darkgreen") +
    xlab('Year') + ylab('Forest loss') +
    scale_x_date(breaks=as.Date(paste0('1/1/', c(2000, 2004, 2008, 2012)), '%m/%d/%Y'),
                 labels=date_format("%Y")) +
    scale_y_continuous(labels=percent_format())
ggsave(paste0(ISO_2, '_national_forest_loss.png'), width=3, height=2, 
       dpi=PLOT_DPI)

stop("UNFINISHED BELOW HERE")

###############################################################################
# Loss
loss <- foreach (forest_threshold=forest_thresholds, .combine=rbind, 
                 .inorder=FALSE, .packages=c("stringr")) %do% {
    this_output_folder <- file.path(prefix, output_folder,
                                    paste0(gsub('_', '', utm_string), '_', 
                                           forest_threshold, 'pct'))
    if (!file_test('-d', this_output_folder)) {
        stop(paste(this_output_folder, 'does not exist'))
    }

    loss_stats_files <- dir(this_output_folder, pattern='_stats_loss.csv', 
                            full.name=TRUE)
    loss_stats <- foreach (loss_stats_file=loss_stats_files, .combine=rbind, 
             .inorder=FALSE) %do% {
        sitecode <- str_extract(basename(loss_stats_file), '^[A-Z]{2,3}')
        these_loss_stats <- read.csv(loss_stats_file)
        these_loss_stats <- cbind(sitecode=sitecode, 
                                  threshold=forest_threshold, these_loss_stats)
        return(these_loss_stats)
    }
}

loss$threshold <- ordered(loss$threshold, levels=unique(loss$threshold), 
                          labels=paste0(unique(loss$threshold), "%"))
# Add some more data fields needed for plotting
loss$year <- as.Date(paste0(loss$year, '-1-1'))
sitecode_key <- read.csv(file.path(prefix, 'TEAM/Sitecode_Key/sitecode_key.csv'))
loss <- merge(loss, sitecode_key)
# Order the aoi names by buffer distance so they will show up properly in plots
aoi_names <- str_extract(unique(as.character(loss$aoi)), "^[a-zA-Z]*")
aoi_buffers <- as.numeric(str_extract(unique(as.character(loss$aoi)), "[0-9]*$"))
aoi_levels <- unique(as.character(loss$aoi))[order(aoi_names, aoi_buffers)]
loss$aoi <- ordered(loss$aoi, levels=aoi_levels)

# Add cumulative and percent loss fields
loss <- group_by(loss, threshold, sitecode, aoi) %>%
    arrange(year) %>%
    mutate(loss_cum=c(NA, cumsum(loss[2:length(loss)])),
           loss_pct=(loss/cover[1])*100)
loss <- group_by(loss, threshold, sitecode, aoi) %>%
    arrange(year) %>%
    mutate(loss_pct_cum=c(NA, cumsum(loss_pct[2:length(loss_pct)])))

###############################################################################
# Gain
gain <- foreach (forest_threshold=forest_thresholds, .combine=rbind, 
                 .inorder=FALSE, .packages=c("stringr")) %do% {
    this_output_folder <- file.path(prefix, output_folder,
                                    paste0(gsub('_', '', utm_string), '_', 
                                           forest_threshold, 'pct'))
    if (!file_test('-d', this_output_folder)) {
        stop(paste(this_output_folder, 'does not exist'))
    }

    gain_stats_files <- dir(this_output_folder, pattern='_stats_gain.csv',
                             full.name=TRUE)
    gain_stats <- foreach (gain_stats_file=gain_stats_files, .combine=rbind, 
             .inorder=FALSE) %do% {
        sitecode <- str_extract(basename(gain_stats_file), '^[A-Z]{2,3}')
        these_gain_stats <- read.csv(gain_stats_file)
        these_gain_stats <- cbind(sitecode=sitecode, 
                                  threshold=forest_threshold, these_gain_stats)
        return(these_gain_stats)
    }
}
gain$threshold <- ordered(gain$threshold, levels=unique(gain$threshold), 
                          labels=paste0(unique(gain$threshold), "%"))
# Add some more data fields needed for plotting
gain <- merge(gain, sitecode_key)
# Order the aoi names by buffer distance so they will show up properly in plots
aoi_names <- str_extract(unique(as.character(gain$aoi)), "^[a-zA-Z]*")
aoi_buffers <- as.numeric(str_extract(unique(as.character(gain$aoi)), "[0-9]*$"))
aoi_levels <- unique(as.character(gain$aoi))[order(aoi_names, aoi_buffers)]
gain$aoi <- ordered(gain$aoi, levels=aoi_levels)

# Calculate total loss, gain, and lossgain per site
summ_stats <- melt(gain, id.vars=c('sitecode', 'threshold', 'aoi'), 
                   measure.vars=c('gain', 'lossgain'))
summ_stats <- tbl_df(summ_stats)
total_loss <- group_by(loss, sitecode, threshold, aoi) %>%
    summarize(variable='loss', value=sum(loss, na.rm=TRUE))
summ_stats <- rbind(summ_stats, total_loss)
summ_stats <- arrange(summ_stats, sitecode, threshold, aoi, variable)

summ_stats$variable <- factor(summ_stats$variable,
                              levels=c('loss', 'lossgain', 'gain'), 
                              labels=c('Loss', 'Loss & gain', 'Gain'))

# Normalize by initial cover, then multiply by 100, to get percentage of 
# initial forest cover
initial_cover <- loss[loss$year == as.Date('2000-1-1'), ]
initial_cover_row <- match(paste(summ_stats$sitecode, summ_stats$aoi, summ_stats$threshold),
                           paste(initial_cover$sitecode, initial_cover$aoi, initial_cover$threshold))
summ_stats$value_pct <- with(summ_stats, (value / initial_cover$cover[initial_cover_row])*100)

save(loss, file="loss.RData")
write.csv(loss, file="loss.csv", row.names=FALSE)
save(summ_stats, file="summ_stats.RData")
write.csv(summ_stats, file="summ_stats.csv", row.names=FALSE)
save(gain, file="gain.RData")
write.csv(gain, file="gain.csv", row.names=FALSE)

###############################################################################
# Make plots
p <- ggplot(filter(loss, aoi == 'ZOI_0', year != as.Date('2000-1-1')),
            aes(year, loss)) +
    geom_line(aes(color=threshold, linetype=threshold)) + 
    geom_point(aes(color=threshold, shape=threshold)) +
    facet_wrap(~ sitecode) +
    xlab('Year') + ylab('Forest loss (ha)') +
    guides(colour=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    guides(linetype=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    guides(shape=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    theme(legend.position="bottom")
ggsave(paste0('all_thresh_forest_loss.png'), width=width, height=height, 
       dpi=dpi, plot=p)

p <- ggplot(filter(loss, aoi == 'ZOI_0', year != as.Date('2000-1-1')),
            aes(year, loss_cum)) +
    geom_line(aes(color=threshold, linetype=threshold)) + 
    geom_line(aes(color=threshold, linetype=threshold)) + 
    geom_point(aes(color=threshold, shape=threshold)) +
    facet_wrap(~ sitecode) +
    xlab('Year') + ylab('Forest loss (ha, cumulative)') +
    guides(colour=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    guides(linetype=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    guides(shape=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    theme(legend.position="bottom")
ggsave(paste0('all_thresh_forest_loss_cum.png'), width=width, 
       height=height, dpi=dpi, plot=p)

p <- ggplot(filter(loss, aoi == 'ZOI_0', year != as.Date('2000-1-1')),
            aes(year, loss_cum)) +
    geom_line(aes(color=threshold, linetype=threshold)) + 
    geom_line(aes(color=threshold, linetype=threshold)) + 
    geom_point(aes(color=threshold, shape=threshold)) +
    facet_wrap(~ sitecode, scales="free_y") +
    xlab('Year') + ylab('Forest loss (ha, cumulative)') +
    guides(colour=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    guides(linetype=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    guides(shape=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    theme(legend.position="bottom")
ggsave(paste0('all_thresh_forest_loss_cum_freey.png'), width=width, 
       height=height, dpi=dpi, plot=p)

p <- ggplot(filter(loss, aoi == 'ZOI_0', year != as.Date('2000-1-1')),
            aes(year, loss_pct/100)) +
    geom_line(aes(color=threshold, linetype=threshold)) + 
    geom_point(aes(color=threshold, shape=threshold)) +
    facet_wrap(~ sitecode) +
    scale_y_continuous(labels=percent_format()) +
    xlab('Year') + ylab('Forest loss (fraction of 2000 forest cover)') +
    guides(colour=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    guides(linetype=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    guides(shape=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    theme(legend.position="bottom")
ggsave(paste0('all_thresh_forest_loss_norm.png'), width=width, height=height, 
       dpi=dpi, plot=p)

p <- ggplot(filter(loss, aoi == 'ZOI_0', year != as.Date('2000-1-1')),
            aes(year, loss_pct_cum/100)) +
    geom_line(aes(color=threshold, linetype=threshold)) + 
    geom_point(aes(color=threshold, shape=threshold)) +
    facet_wrap(~ sitecode) +
    scale_y_continuous(labels=percent_format()) +
    xlab('Year') + ylab('Forest loss (cumulative fraction of 2000 forest cover)') +
    guides(colour=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    guides(linetype=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    guides(shape=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    theme(legend.position="bottom")
ggsave(paste0('all_thresh_forest_loss_norm_cum.png'), width=width, 
       height=height, dpi=dpi, plot=p)

p <- ggplot(filter(loss, aoi == 'ZOI_0', year != as.Date('2000-1-1')),
            aes(year, loss_pct_cum/100)) +
    geom_line(aes(color=threshold, linetype=threshold)) + 
    geom_point(aes(color=threshold, shape=threshold)) +
    facet_wrap(~ sitecode, scales="free_y") +
    scale_y_continuous(labels=percent_format()) +
    xlab('Year') + ylab('Forest loss (cumulative fraction of 2000 forest cover)') +
    guides(colour=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    guides(linetype=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    guides(shape=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    theme(legend.position="bottom")
ggsave(paste0('all_thresh_forest_loss_norm_cum_freey.png'), width=width, 
       height=height, dpi=dpi, plot=p)

###############################################################################
# Make plots by AOI
p <- ggplot(filter(loss, threshold == '75%', year != as.Date('2000-1-1'), grepl("CSA", aoi)),
            aes(year, loss_pct_cum/100)) +
    stat_summary(fun.y=mean, geom="line", aes(group=as.character(aoi), colour=aoi, linetype=aoi)) +
    scale_y_continuous(labels=percent_format()) +
    xlab('Year') +
    ylab('Forest loss (cumulative fraction of 2000 forest cover)') +
    guides(colour=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    guides(linetype=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    guides(shape=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    theme(legend.position="bottom")
ggsave(paste0('all_thresh_forest_loss_norm_cum_aoi_CSA.png'), width=width, 
       height=height, dpi=dpi, plot=p)

p <- ggplot(filter(loss, threshold == '75%', year != as.Date('2000-1-1'), grepl("PA", aoi)),
            aes(year, loss_pct_cum/100)) +
    stat_summary(fun.y=mean, geom="line", aes(group=as.character(aoi), colour=aoi, linetype=aoi)) +
    scale_y_continuous(labels=percent_format()) +
    xlab('Year') +
    ylab('Forest loss (cumulative fraction of 2000 forest cover)') +
    guides(colour=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    guides(linetype=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    guides(shape=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    theme(legend.position="bottom")
ggsave(paste0('all_thresh_forest_loss_norm_cum_aoi_PA.png'), width=width, 
       height=height, dpi=dpi, plot=p)

p <- ggplot(filter(loss, threshold == '75%', year != as.Date('2000-1-1'), grepl("ZOI", aoi)),
            aes(year, loss_pct_cum/100)) +
    stat_summary(fun.y=mean, geom="line", aes(group=as.character(aoi), colour=aoi, linetype=aoi)) +
    scale_y_continuous(labels=percent_format()) +
    xlab('Year') +
    ylab('Forest loss (cumulative fraction of 2000 forest cover)') +
    guides(colour=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    guides(linetype=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    guides(shape=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    theme(legend.position="bottom")
ggsave(paste0('all_thresh_forest_loss_norm_cum_aoi_ZOI.png'), width=width, 
       height=height, dpi=dpi, plot=p)

###############################################################################
# Make plots by site type
p <- ggplot(filter(loss, year != as.Date('2000-1-1'), aoi %in% c("ZOI_0", "CSA_0", "PA_0")),
            aes(year, loss_pct_cum/100, colour=sitetype, linetype=sitetype, group=sitecode)) +
    #stat_summary(fun.y=mean, geom="line", aes(group=as.character(sitetype), colour=sitetype, linetype=sitetype)) +
    geom_line() + geom_point() +
    scale_y_continuous(labels=percent_format()) +
    facet_grid(threshold~aoi) +
    xlab('Year') +
    ylab('Forest loss (cumulative fraction of 2000 forest cover)') +
    guides(colour=guide_legend(title="Site type", keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    guides(linetype=guide_legend(title="Site type", keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    guides(shape=guide_legend(title="Site type", keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    theme(legend.position="bottom")
ggsave(paste0('all_thresh_forest_loss_norm_cum_sitetype.png'), width=width, 
       height=height, dpi=dpi, plot=p)

p <- ggplot(filter(loss, year != as.Date('2000-1-1'), aoi %in% c("ZOI_0", "CSA_0", "PA_0")),
            aes(year, loss_pct_cum/100, colour=sitetype, linetype=sitetype, group=sitecode)) +
    #stat_summary(fun.y=mean, geom="line", aes(group=as.character(sitetype), colour=sitetype, linetype=sitetype)) +
    geom_line() + geom_point() +
    scale_y_continuous(labels=percent_format()) +
    facet_grid(threshold~aoi) +
    xlab('Year') +
    ylab('Forest loss (cumulative fraction of 2000 forest cover)') +
    guides(colour=guide_legend(title="Site type", keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    guides(linetype=guide_legend(title="Site type", keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    guides(shape=guide_legend(title="Site type", keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    theme(legend.position="bottom")
ggsave(paste0('all_thresh_forest_loss_norm_cum_sitetype_noPSH.png'), width=width, 
       height=height, dpi=dpi, plot=p)

###############################################################################
# Make plots by continent
p <- ggplot(filter(loss, threshold == '75%', year != as.Date('2000-1-1'), aoi %in% c("ZOI_0", "CSA_0", "PA_0")),
            aes(year, loss_pct_cum/100, colour=continent, linetype=continent, group=sitecode)) +
    geom_line() + geom_point() +
    scale_y_continuous(labels=percent_format()) +
    facet_grid(~aoi) +
    xlab('Year') +
    ylab('Forest loss (cumulative fraction of 2000 forest cover)') +
    guides(colour=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    guides(linetype=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    guides(shape=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    theme(legend.position="bottom")
ggsave(paste0('all_thresh_forest_loss_norm_cum_continent.png'), width=width, 
       height=height, dpi=dpi, plot=p)

p <- ggplot(filter(loss, threshold == '75%', year != as.Date('2000-1-1'), sitecode != 'PSH', aoi %in% c("ZOI_0", "CSA_0", "PA_0")),
            aes(year, loss_pct_cum/100, colour=continent, linetype=continent, group=sitecode)) +
    geom_line() + geom_point() +
    scale_y_continuous(labels=percent_format()) +
    facet_grid(~aoi) +
    xlab('Year') +
    ylab('Forest loss (cumulative fraction of 2000 forest cover)') +
    guides(colour=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    guides(linetype=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    guides(shape=guide_legend(keywidth=2.5, keyheight=1, ncol=4, direction='horizontal')) +
    theme(legend.position="bottom")
ggsave(paste0('all_thresh_forest_loss_norm_cum_continent_noPSH.png'), width=width, 
       height=height, dpi=dpi, plot=p)

###############################################################################
# Make loss/gain plots

p <- ggplot(filter(summ_stats, aoi == 'ZOI_0'),
            aes(threshold, value, fill=variable)) +
    geom_bar(stat='identity', position="dodge") +
    facet_grid(. ~ threshold) +
    facet_wrap(~ sitecode) +
    scale_fill_manual("Forest Change Type",
                      breaks=c('Loss',
                               'Loss & gain',
                               'Gain'),
                      values=c('#ff0000',
                               '#ff00ff',
                               '#0000ff')) +
    xlab('Forest threshold (% tree cover)') +
    ylab('Change from 2000-2012 (ha)')
ggsave(paste0('all_thresh_forest_gainAndLoss_2000-2012.png'), width=width, 
       height=height, dpi=dpi, plot=p)

p <- ggplot(filter(summ_stats, aoi == 'ZOI_0'),
            aes(threshold, value_pct/100, fill=variable)) +
    geom_bar(stat='identity', position="dodge") +
    facet_grid( ~ threshold) +
    facet_wrap(~ sitecode) +
    scale_fill_manual("Forest Change Type",
                      breaks=c('Loss',
                               'Loss & gain',
                               'Gain'),
                      values=c('#ff0000',
                               '#ff00ff',
                               '#0000ff')) +
    scale_y_continuous(labels=percent_format()) +
    xlab('Forest threshold (% tree cover)') +
#    ylim(c('0%','50%')) +
    ylab('Change from 2000-2012 (as fraction of 2000 forest cover)')
ggsave(paste0('all_thresh_forest_gainAndLoss_2000-2012_norm.png'), width=width, 
       height=height, dpi=dpi, plot=p)
