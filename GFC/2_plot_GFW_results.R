source('../0_settings.R')

library(ggplot2)
library(scales)
library(dplyr)

source(file.path('..', '0_settings.R'))

in_folder <- file.path(prefix, "GRP", "GFC", "GFW")

###############################################################################
# Setup parameters
###############################################################################

forest_threshold <- c(10)

data_folder <- file.path(prefix, "GRP", "GFC", "GFW")
plot_folder <- file.path(prefix, "GRP", "GFC", "GFW")
stopifnot(file_test('-d', data_folder))

ISO_2s <- c("ET", "NE", "ER", "ID", "UG")

for (ISO_2 in ISO_2s) {
    gfc_data <- read.csv(file.path(in_folder, paste0(ISO_2, "_GFC.csv")))

    gfc_data$min_percent_canopy_density <- factor(gfc_data$min_percent_canopy_density)
    gfc_data$year <- as.Date(paste0('1/1/', gfc_data$year), '%m/%d/%Y')

    # Plot by region
    ggplot(filter(gfc_data, min_percent_canopy_density == forest_threshold)) +
        geom_line(aes(year, loss_percent, colour=region))

    # Calculate national-level loss
    gfc_data_natl <- group_by(gfc_data, min_percent_canopy_density, year) %>%
        summarize(extent_ha=sum(extent_ha),
                  loss_ha=sum(loss_ha)) %>%
        mutate(loss_percent=c(NA, loss_percent=(loss_ha[2:length(loss_ha)]/extent_ha[1])*100),
               loss_km2=c(NA, loss_ha[2:length(loss_ha)]*.01))

    ggplot(gfc_data_natl) + geom_line(aes(year, loss_percent, colour=min_percent_canopy_density))

    print(ISO_2)
    print(sum(filter(gfc_data_natl, min_percent_canopy_density == 20)$loss_km2, na.rm=TRUE))

    ggplot(filter(gfc_data_natl, min_percent_canopy_density == 20)) +
        theme_bw() +
        geom_line(aes(year, loss_percent / 100), colour="darkgreen") +
        xlab('Year') + ylab('Forest loss') +
        scale_x_date(breaks=as.Date(paste0('1/1/', c(2000, 2004, 2008, 2012)), '%m/%d/%Y'),
                     labels=date_format("%Y")) +
        scale_y_continuous(labels=percent_format())
    ggsave(paste0(ISO_2, '_national_forest_loss_pct.png'), width=3, height=2, 
           dpi=PLOT_DPI)
    ggplot(filter(gfc_data_natl, min_percent_canopy_density == 20)) +
        theme_bw() +
        geom_line(aes(year, loss_percent / 100), colour="darkgreen") +
        xlab('Year') + ylab('Forest loss') +
        scale_x_date(breaks=as.Date(paste0('1/1/', c(2000, 2004, 2008, 2012)), '%m/%d/%Y'),
                     labels=date_format("%Y")) +
        scale_y_continuous(labels=percent_format())
    ggsave(paste0(ISO_2, '_national_forest_loss_pct.eps'), width=3, height=2, 
           dpi=PLOT_DPI)

    ggplot(filter(gfc_data_natl, min_percent_canopy_density == 20)) +
        theme_bw() +
        geom_line(aes(year, loss_km2), colour="darkgreen") +
        xlab('Year') + ylab(expression("Forest loss (km"^2*")")) +
        scale_x_date(breaks=as.Date(paste0('1/1/', c(2000, 2004, 2008, 2012)), '%m/%d/%Y'),
                     labels=date_format("%Y"))
    ggsave(paste0(ISO_2, '_national_forest_loss_km2.png'), width=3, height=2, 
           dpi=PLOT_DPI)
    ggplot(filter(gfc_data_natl, min_percent_canopy_density == 20)) +
        theme_bw() +
        geom_line(aes(year, loss_km2), colour="darkgreen") +
        xlab('Year') + ylab(expression("Forest loss (km"^2*")")) +
        scale_x_date(breaks=as.Date(paste0('1/1/', c(2000, 2004, 2008, 2012)), '%m/%d/%Y'),
                     labels=date_format("%Y"))
    ggsave(paste0(ISO_2, '_national_forest_loss_km2.eps'), width=3, height=2, 
           dpi=PLOT_DPI)

}
